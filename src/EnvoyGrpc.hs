module EnvoyGrpc ( runServer ) where

import Data.IORef
import Data.Functor( ($>) )
import Data.Map.Strict( Map )
import Data.Maybe( fromMaybe, maybeToList )
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
import Proto.Envoy.Api.V2.Discovery( DiscoveryRequest, DiscoveryResponse )
import Proto.Envoy.Api.V2.Core.Address( Address )
import Proto.Envoy.Api.V2.Endpoint( ClusterLoadAssignment )
import Proto.Envoy.Api.V2.Listener.ListenerComponents( FilterChain )
import Proto.Envoy.Config.Filter.Network.HttpConnectionManager.V2.HttpConnectionManager
  ( HttpConnectionManager )
import Proto.Envoy.Service.Discovery.V2.Ads( AggregatedDiscoveryService )
import Proto.Google.Protobuf.Any( Any )

import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import qualified Proto.Envoy.Api.V2.Core.Address as AddressV2
import qualified Proto.Envoy.Api.V2.Route.RouteComponents as RouteV2
import qualified Proto.Envoy.Api.V2.Cluster as ClusterV2
import qualified Proto.Envoy.Api.V2.Listener as ListenerV2
import qualified System.Log.Logger as L

import Ast
import Config( AdsConfig(..) )

runServer
  :: AdsConfig
  -> Map Text (Listener [Rule])
  -> Map Text Destination
  -> IO ()
runServer AdsConfig{..} lsts dsts =
  runGrpc ts ss [ads (renderClusters dsts) (renderListeners lsts)] []
  where
    ts = tlsSettings certificate key
    ss = setLogger l . setTimeout 300 . setHost (fromString host) . setPort port $ defaultSettings
    l r _ _ = L.debugM "EnvoyGrpc" ("ADS client disconnected: " ++ show (remoteHost r))

data AdsState = Wait | Term | Pass DiscoveryRequest

ads :: DiscoveryResponse -> DiscoveryResponse -> ServiceHandler
ads dsts lsts = bidiStream
  (RPC :: RPC AggregatedDiscoveryService "streamAggregatedResources")
  (adsHandler dsts lsts)

adsHandler
  :: DiscoveryResponse
  -> DiscoveryResponse
  -> BiDiStreamHandler IO DiscoveryRequest DiscoveryResponse AdsState
adsHandler dsts lsts req = do
  _ <- L.debugM "EnvoyGrpc" ("New ADS client: " ++ show (remoteHost req))
  ref <- newIORef (False, False)
  pure (Wait, BiDiStream $ s ref)
  where
    s
      :: IORef (Bool, Bool)
      -> AdsState
      -> IO (BiDiStep IO DiscoveryRequest DiscoveryResponse AdsState)
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

renderListeners :: Map Text (Listener [Rule]) -> DiscoveryResponse
renderListeners ls = defMessage
  & field @"typeUrl" .~ listenerUrl
  & field @"resources" .~ M.foldrWithKey (\k v a -> f k v : a) [] ls
  where
    f :: Text -> Listener [Rule] -> Any
    f name Listener{..} = defMessage
      & field @"typeUrl" .~ listenerUrl
      & field @"value" .~ encodeMessage ((defMessage :: ListenerV2.Listener)
        & field @"name" .~ name
        & field @"address" .~ (defMessage
          & field @"socketAddress" .~ (defMessage
            & field @"address" .~ host
            & field @"portValue" .~ fromIntegral port
          )
        )
        & field @"filterChains" .~ [envoyRoutes http]
      )

envoyRoutes :: [Rule] -> FilterChain
envoyRoutes routes = defMessage
  & field @"filters" .~ [ defMessage
    & field @"name" .~ "envoy.http_connection_manager"
    & field @"typedConfig" .~ (defMessage
      & field @"typeUrl" .~ httpConnectionManagerUrl
      & field @"value" .~ encodeMessage ((defMessage :: HttpConnectionManager)
        & field @"statPrefix" .~ "http_connection_manager" -- TODO
        & field @"httpFilters" .~ [ defMessage & field @"name" .~ "envoy.router" ]
        & field @"routeConfig" .~ (defMessage
          & field @"virtualHosts" .~ [defMessage
            & field @"name" .~ "virtual_host" -- TODO
            & field @"domains" .~ ["*"]
            & field @"routes" .~ map envoyRoute routes
          ]
        )
      )
    )
  ]

envoyRoute :: Rule -> RouteV2.Route
envoyRoute Rule{..} = defMessage
  & field @"match" .~ (addPathMatcher defMessage (fromMaybe (Prefix "/") path)
    & field @"headers" .~
      map (\(n, m) -> addHeaderMatcher m $ defMessage & field @"name" .~ n) allHeaders
  )
  & maybe
      (field @"directResponse" .~ (defMessage
        & field @"status" .~ 404
        & field @"body" .~ (defMessage & field @"inlineString" .~ "no route")
      ))
      (\t -> field @"route" .~ (defMessage & field @"cluster" .~ t))
      action
  where
    addPathMatcher :: RouteV2.RouteMatch -> Matcher -> RouteV2.RouteMatch
    addPathMatcher msg (Exact t) = msg & field @"path" .~ t
    addPathMatcher msg (Prefix t) = msg & field @"prefix" .~ t

    addHeaderMatcher :: Matcher -> RouteV2.HeaderMatcher -> RouteV2.HeaderMatcher
    addHeaderMatcher (Exact t) msg = msg & field @"exactMatch" .~ t
    addHeaderMatcher (Prefix t) msg = msg & field @"prefixMatch" .~ t

    allHeaders =
      fmap (":authority",) (maybeToList authority) ++
      fmap (":method",) (maybeToList method) ++
      headers

renderClusters :: Map Text Destination -> DiscoveryResponse
renderClusters ds = defMessage
  & field @"typeUrl" .~ clusterUrl
  & field @"resources" .~ map f (M.toList ds)
  where
    f :: (Text, Destination) -> Any
    f (name, dst) = defMessage
      & field @"typeUrl" .~ clusterUrl
      & field @"value" .~ encodeMessage (cluster name dst
        & field @"name" .~ name
        & field @"connectTimeout" .~ (defMessage & field @"seconds" .~ 5)
      )

    loadAssignment name hs = (defMessage :: ClusterLoadAssignment)
      & field @"clusterName" .~ name
      & field @"endpoints" .~ [defMessage
        & field @"lbEndpoints" .~ map (\(host, port) -> defMessage
          & field @"endpoint" .~ (defMessage
            & field @"address" .~ ((defMessage :: AddressV2.Address)
              & field @"socketAddress" .~ (defMessage
                & field @"address" .~ host
                & field @"portValue" .~ fromIntegral (port :: Int)
              )
            )
          )
        ) hs
      ]

    clusterType :: Discovery -> ClusterV2.Cluster
    clusterType Static = defMessage & field @"type'" .~ ClusterV2.Cluster'STATIC
    clusterType StrictDns = defMessage & field @"type'" .~ ClusterV2.Cluster'STRICT_DNS
    clusterType LogicalDns = defMessage & field @"type'" .~ ClusterV2.Cluster'LOGICAL_DNS

    cluster :: Text -> Destination -> ClusterV2.Cluster
    cluster name Destination{..} =
      clusterType discovery & field @"loadAssignment" .~ loadAssignment name hosts
