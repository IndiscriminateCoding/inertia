module EnvoyGrpc ( runServer ) where

import Data.IORef
import Data.Functor( ($>) )
import Data.Map.Strict( Map )
import Data.Maybe( fromMaybe, isNothing, maybeToList )
import Data.ProtoLens.Encoding( encodeMessage )
import Data.ProtoLens.Field( field )
import Data.ProtoLens.Message( defMessage )
import Data.String( fromString )
import Data.Text( Text )
import Lens.Family2( view, (&), (.~) )
import Network.GRPC.HTTP2.ProtoLens( RPC(..) )
import Network.GRPC.Server
  ( BiDiStep(..)
  , BiDiStream(..)
  , BiDiStreamHandler
  , ServiceHandler
  , bidiStream
  , runGrpc )
import Network.Wai( remoteHost )
import Network.Wai.Handler.Warp
  ( Settings
  , defaultSettings
  , setHost
  , setLogger
  , setPort
  , setTimeout )
import Network.Wai.Handler.WarpTLS( TLSSettings, tlsSettings )

import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Proto.Envoy.Config.Cluster.V3.OutlierDetection as Envoy
import qualified Proto.Envoy.Config.Cluster.V3.Cluster as Envoy
import qualified Proto.Envoy.Config.Core.V3.Address as Envoy
import qualified Proto.Envoy.Config.Core.V3.Base as EnvoyBase
import qualified Proto.Envoy.Config.Endpoint.V3.Endpoint as Envoy
import qualified Proto.Envoy.Config.Listener.V3.Listener as Envoy
import qualified Proto.Envoy.Config.Listener.V3.ListenerComponents as Envoy
import qualified Proto.Envoy.Config.Route.V3.Route as Envoy
import qualified Proto.Envoy.Config.Route.V3.RouteComponents as Envoy
import qualified
  Proto.Envoy.Extensions.Filters.Network.HttpConnectionManager.V3.HttpConnectionManager as Envoy
import qualified Proto.Envoy.Extensions.TransportSockets.Tls.V3.Cert as Envoy
import qualified Proto.Envoy.Service.Discovery.V3.Ads as Envoy
import qualified Proto.Envoy.Service.Discovery.V3.Discovery as Envoy
import qualified Proto.Envoy.Type.V3.Percent as Envoy
import qualified Proto.Google.Protobuf.Any as Google
import qualified Proto.Google.Protobuf.Duration as Google
import qualified Proto.Google.Protobuf.Wrappers as Google
import qualified System.Log.Logger as L

import Ast
import Config( AdsConfig(..) )
import Duration

runServer
  :: AdsConfig
  -> Map Text (Listener [Rule])
  -> Map Text Destination
  -> IO ()
runServer AdsConfig{..} lsts dsts =
  runGrpc ts ss [ads (renderClusters dsts) (renderListeners dsts lsts)] []
  where
    ts = tlsSettings certificate key
    ss = setLogger l . setTimeout 300 . setHost (fromString host) . setPort port $ defaultSettings
    l r s _ = L.debugM "EnvoyGrpc" $ concat
      [ "gRPC request terminated ("
      , show s
      , "): "
      , show (remoteHost r) ]

data AdsState = Wait | Term | Pass Envoy.DiscoveryRequest

ads :: Envoy.DiscoveryResponse -> Envoy.DiscoveryResponse -> ServiceHandler
ads dsts lsts = bidiStream
  (RPC :: RPC Envoy.AggregatedDiscoveryService "streamAggregatedResources")
  (adsHandler dsts lsts)

adsHandler
  :: Envoy.DiscoveryResponse
  -> Envoy.DiscoveryResponse
  -> BiDiStreamHandler IO Envoy.DiscoveryRequest Envoy.DiscoveryResponse AdsState
adsHandler dsts lsts req = do
  _ <- L.debugM "EnvoyGrpc" ("New ADS client: " ++ show (remoteHost req))
  ref <- newIORef (False, False)
  pure (Wait, BiDiStream $ s ref)
  where
    s
      :: IORef (Bool, Bool)
      -> AdsState
      -> IO (BiDiStep IO Envoy.DiscoveryRequest Envoy.DiscoveryResponse AdsState)
    s _ Wait = pure (WaitInput (const $ pure . Pass) (const . pure $ Term))
    s _ Term = L.debugM "EnvoyGrpc" ("Client EOF: " ++ show (remoteHost req)) $> Abort
    s r (Pass dr) = do
      (l, d) <- readIORef r
      case view (field @"typeUrl") dr of
        url | url == listenerUrl ->
          if l then s r Wait
          else writeIORef r (True, d) $> WriteOutput Wait lsts
        url | url == clusterUrl ->
          if d then s r Wait
          else writeIORef r (l, True) $> WriteOutput Wait dsts
        url -> putStrLn ("Unknown typeUrl: " ++ show url) >> s r Wait

listenerUrl :: Text
listenerUrl = "type.googleapis.com/envoy.api.v2.Listener"

clusterUrl :: Text
clusterUrl = "type.googleapis.com/envoy.api.v2.Cluster"

httpConnectionManagerUrl :: Text
httpConnectionManagerUrl = Text.concat
  [ "type.googleapis.com/"
  , "envoy.config.filter.network.http_connection_manager.v2.HttpConnectionManager" ]

upstreamTlsContextUrl :: Text
upstreamTlsContextUrl =
  "type.googleapis.com/envoy.extensions.transport_sockets.tls.v3.UpstreamTlsContext"

renderListeners :: Map Text Destination -> Map Text (Listener [Rule]) -> Envoy.DiscoveryResponse
renderListeners ds ls = defMessage
  & field @"typeUrl" .~ listenerUrl
  & field @"resources" .~ M.foldrWithKey (\k v a -> f k v : a) [] ls
  where
    f :: Text -> Listener [Rule] -> Google.Any
    f name Listener{..} = defMessage
      & field @"typeUrl" .~ listenerUrl
      & field @"value" .~ encodeMessage ((defMessage :: Envoy.Listener)
        & field @"name" .~ name
        & field @"address" .~ (defMessage
          & field @"socketAddress" .~ (defMessage
            & field @"address" .~ host
            & field @"portValue" .~ fromIntegral port
          )
        )
        & field @"filterChains" .~ [envoyRoutes ds http]
      )

envoyRoutes :: Map Text Destination -> [Rule] -> Envoy.FilterChain
envoyRoutes dsts routes = defMessage
  & field @"filters" .~ [ defMessage
    & field @"name" .~ "envoy.http_connection_manager"
    & field @"typedConfig" .~ (defMessage
      & field @"typeUrl" .~ httpConnectionManagerUrl
      & field @"value" .~ encodeMessage ((defMessage :: Envoy.HttpConnectionManager)
        & field @"statPrefix" .~ "http_connection_manager" -- TODO
        & field @"httpFilters" .~ [ defMessage & field @"name" .~ "envoy.filters.http.router" ]
        & field @"upgradeConfigs" .~ [ defMessage & field @"upgradeType" .~ "websocket" ]
        & field @"commonHttpProtocolOptions" .~ (defMessage
          & field @"idleTimeout" .~ protobufDuration (seconds 600)
        )
        & field @"routeConfig" .~ (defMessage
          & field @"virtualHosts" .~ [defMessage
            & field @"name" .~ "virtual_host" -- TODO
            & field @"domains" .~ ["*"]
            & field @"routes" .~ map (envoyRoute dsts) routes
          ]
        )
      )
    )
  ]

envoyRoute :: Map Text Destination -> Rule -> Envoy.Route
envoyRoute dsts Rule{..} = defMessage
  & field @"match" .~ (addPathMatcher defMessage (fromMaybe (Prefix "/") path)
    & field @"headers" .~
      map (\(n, m) -> addHeaderMatcher m $ defMessage & field @"name" .~ n) allHeaders
  )
  & addAction action
  where
    addAction :: Maybe Text -> Envoy.Route -> Envoy.Route
    addAction Nothing r = r
      & field @"directResponse" .~ (defMessage
        & field @"status" .~ 404
        & field @"body" .~ (defMessage & field @"inlineString" .~ "no route")
      )
    addAction (Just c) r = r
      & field @"route" .~ (defMessage
        & field @"cluster" .~ c
        & field @"maybe'timeout" .~ fmap protobufDuration (M.lookup c dsts >>= requestTimeout)
        & field @"maybe'retryPolicy" .~ fmap makeRetryPolicy (M.lookup c dsts >>= retryPolicy)
      )

    makeRetryPolicy :: RetryPolicy -> Envoy.RetryPolicy
    makeRetryPolicy RetryPolicy{..} = defMessage
      & field @"retryOn" .~
        "gateway-error,reset,connect-failure,refused-stream,retriable-status-codes"
      & field @"numRetries" .~ uint32 numRetries
      & field @"maybe'perTryTimeout" .~ fmap protobufDuration perTryTimeout
      & field @"retriableStatusCodes" .~ fmap fromIntegral retriableStatusCodes
      & field @"maybe'retryBackOff" .~ fmap
        (\RetryBackOff{..} -> defMessage
          & field @"baseInterval" .~ protobufDuration baseInterval
          & field @"maxInterval" .~ protobufDuration maxInterval
        )
        retryBackOff

    addPathMatcher :: Envoy.RouteMatch -> Matcher -> Envoy.RouteMatch
    addPathMatcher msg (Exact t) = msg & field @"path" .~ t
    addPathMatcher msg (Prefix t) = msg & field @"prefix" .~ t

    addHeaderMatcher :: Matcher -> Envoy.HeaderMatcher -> Envoy.HeaderMatcher
    addHeaderMatcher (Exact t) msg = msg & field @"exactMatch" .~ t
    addHeaderMatcher (Prefix t) msg = msg & field @"prefixMatch" .~ t

    allHeaders =
      fmap (":authority",) (maybeToList authority) ++
      fmap (":method",) (maybeToList method) ++
      headers

uint32 :: Integral n => n -> Google.UInt32Value
uint32 n = defMessage & field @"value" .~ fromIntegral n

percent :: Integral n => n -> Envoy.Percent
percent n = defMessage & field @"value" .~ fromIntegral n

renderClusters :: Map Text Destination -> Envoy.DiscoveryResponse
renderClusters ds = defMessage
  & field @"typeUrl" .~ clusterUrl
  & field @"resources" .~ map f (M.toList ds)
  where
    f :: (Text, Destination) -> Google.Any
    f (name, dst@Destination{..}) = defMessage
      & field @"typeUrl" .~ clusterUrl
      & field @"value" .~ encodeMessage (
        addHttpVersion (httpVersion httpOptions) .
        addLbConfig loadBalancer .
        addCircuitBreaker circuitBreaker .
        addOutlierDetection outlierDetection $
        cluster name dst
          & field @"name" .~ name
          & field @"connectTimeout" .~ protobufDuration connectTimeout
          & field @"lbPolicy" .~ lbPolicy loadBalancer
          & field @"maybe'commonLbConfig" .~
            fmap
              (\t -> defMessage & field @"healthyPanicThreshold" .~ percent t)
              healthyPanicThreshold
          & field @"maybe'transportSocket" .~ fmap transportSocket tls
          & field @"maybe'upstreamHttpProtocolOptions" .~ (
            if any (isNothing . sni) tls
            then Just (defMessage & field @"autoSni" .~ True)
            else Nothing
          )
          & field @"maybe'upstreamConnectionOptions" .~
            fmap
              (\TcpKeepalive{..} -> defMessage & field @"tcpKeepalive" .~ (defMessage
                & field @"keepaliveProbes" .~ uint32 probes
                & field @"keepaliveTime" .~ uint32 (toSeconds time)
                & field @"keepaliveInterval" .~ uint32 (toSeconds interval)
              ))
              tcpKeepalive
          & field @"commonHttpProtocolOptions" .~ (defMessage
            & field @"idleTimeout" .~ protobufDuration (idleTimeout httpOptions)
          )
      )

    transportSocket :: Tls -> EnvoyBase.TransportSocket
    transportSocket Tls{..} = defMessage
      & field @"name" .~ "envoy.transport_sockets.tls"
      & field @"typedConfig" .~ (defMessage
        & field @"typeUrl" .~ upstreamTlsContextUrl
        & field @"value" .~ encodeMessage (maybe
          (defMessage :: Envoy.UpstreamTlsContext)
          (\s -> defMessage & field @"sni" .~ s)
          sni
        )
      )

    addHttpVersion :: HttpVersion -> Envoy.Cluster -> Envoy.Cluster
    addHttpVersion H1 c = c
    addHttpVersion H2 c = c
      & field @"http2ProtocolOptions" .~ defMessage
    addHttpVersion Downstream c = c
      & field @"protocolSelection" .~ Envoy.Cluster'USE_DOWNSTREAM_PROTOCOL

    addOutlierDetection :: Maybe OutlierDetection -> Envoy.Cluster -> Envoy.Cluster
    addOutlierDetection Nothing c = c
    addOutlierDetection (Just OutlierDetection{..}) c = c
      & field @"outlierDetection" .~ (
        addLocalOrigin localOrigin .
        addFailurePercentage failurePercentage .
        addSuccessRate successRate $
        defMessage
          & field @"maybe'interval" .~ fmap protobufDuration interval
          & field @"maybe'baseEjectionTime" .~ fmap protobufDuration baseEjectionTime
          & field @"maybe'maxEjectionPercent" .~ fmap uint32 maxEjectionPercent
          & field @"maybe'consecutive5xx" .~
            fmap (uint32 . num) consecutive5xx
          & field @"enforcingConsecutive5xx" .~
            uint32 (maybe 0 (enforcing :: Consecutive -> Int) consecutive5xx)
          & field @"maybe'consecutiveGatewayFailure" .~
            fmap (uint32 . num) consecutiveGatewayFailure
          & field @"enforcingConsecutiveGatewayFailure" .~
            uint32 (maybe 0 (enforcing :: Consecutive -> Int) consecutiveGatewayFailure)
      )

    addSuccessRate :: Maybe SuccessRate -> Envoy.OutlierDetection -> Envoy.OutlierDetection
    addSuccessRate Nothing x = x
      & field @"enforcingSuccessRate" .~ uint32 (0 :: Int)
    addSuccessRate (Just SuccessRate{..}) x = x
      & field @"successRateMinimumHosts" .~ uint32 minimumHosts
      & field @"successRateRequestVolume" .~ uint32 requestVolume
      & field @"successRateStdevFactor" .~ uint32 stdevFactor
      & field @"enforcingSuccessRate" .~ uint32 enforcing

    addFailurePercentage
      :: Maybe FailurePercentage -> Envoy.OutlierDetection -> Envoy.OutlierDetection
    addFailurePercentage Nothing x = x
      & field @"enforcingFailurePercentage" .~ uint32 (0 :: Int)
    addFailurePercentage (Just FailurePercentage{..}) x = x
      & field @"failurePercentageMinimumHosts" .~ uint32 minimumHosts
      & field @"failurePercentageRequestVolume" .~ uint32 requestVolume
      & field @"failurePercentageThreshold" .~ uint32 threshold
      & field @"enforcingFailurePercentage" .~ uint32 enforcing

    addLocalOrigin :: Maybe LocalOrigin -> Envoy.OutlierDetection -> Envoy.OutlierDetection
    addLocalOrigin Nothing x = x
    addLocalOrigin (Just LocalOrigin{..}) x = x
      & field @"splitExternalLocalOriginErrors" .~ True
      & field @"maybe'consecutiveLocalOriginFailure" .~ fmap (uint32 . num) consecutive
      & field @"enforcingConsecutiveLocalOriginFailure" .~
        uint32 (maybe 0 (enforcing :: Consecutive -> Int) consecutive)
      & field @"enforcingLocalOriginSuccessRate" .~ uint32 (fromMaybe 0 enforcingSuccessRate)
      & field @"enforcingFailurePercentageLocalOrigin"
        .~ uint32 (fromMaybe 0 enforcingFailurePercentage)

    addCircuitBreaker :: Maybe CircuitBreaker -> Envoy.Cluster -> Envoy.Cluster
    addCircuitBreaker (Just CircuitBreaker{..}) c = c
      & field @"circuitBreakers" .~ (defMessage
        & field @"thresholds" .~ [ defMessage
          & field @"maybe'maxConnections" .~ fmap uint32 maxConnections
          & field @"maybe'maxPendingRequests" .~ fmap uint32 maxPendingRequests
          & field @"maybe'maxRequests" .~ fmap uint32 maxRequests
          & field @"maybe'maxRetries" .~ fmap uint32 maxRetries
        ]
      )
    addCircuitBreaker Nothing c = c

    addLbConfig :: LoadBalancer -> Envoy.Cluster -> Envoy.Cluster
    addLbConfig (LeastRequest n) c = c
      & field @"leastRequestLbConfig" .~ (defMessage
        & field @"choiceCount" .~ uint32 n
      )
    addLbConfig _ c = c

    lbPolicy (LeastRequest _) = Envoy.Cluster'LEAST_REQUEST
    lbPolicy Random = Envoy.Cluster'RANDOM
    lbPolicy RoundRobin = Envoy.Cluster'ROUND_ROBIN

    loadAssignment name hs = (defMessage :: Envoy.ClusterLoadAssignment)
      & field @"clusterName" .~ name
      & field @"endpoints" .~ [defMessage
        & field @"lbEndpoints" .~ map (\(host, port) -> defMessage
          & field @"endpoint" .~ (defMessage
            & field @"address" .~ ((defMessage :: Envoy.Address)
              & field @"socketAddress" .~ (defMessage
                & field @"address" .~ host
                & field @"portValue" .~ fromIntegral (port :: Int)
              )
            )
          )
        ) hs
      ]

    clusterType :: Discovery -> Envoy.Cluster
    clusterType Static = defMessage & field @"type'" .~ Envoy.Cluster'STATIC
    clusterType StrictDns = defMessage & field @"type'" .~ Envoy.Cluster'STRICT_DNS
    clusterType LogicalDns = defMessage & field @"type'" .~ Envoy.Cluster'LOGICAL_DNS

    cluster :: Text -> Destination -> Envoy.Cluster
    cluster name Destination{..} =
      clusterType discovery & field @"loadAssignment" .~ loadAssignment name hosts

protobufDuration :: Duration -> Google.Duration
protobufDuration (Duration ms) = defMessage
  & field @"seconds" .~ fromInteger seconds
  & field @"nanos" .~ fromInteger nanos
  where
    seconds = ms `div` 1000
    nanos = (ms - seconds * 1000) * 1_000_000
