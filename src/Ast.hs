module Ast where

import Control.Exception( Exception, throwIO )
import Data.Function( (&) )
import Data.Maybe( isJust )
import Data.Text( Text, toCaseFold )

import Duration( Duration )
import OpenApi( Endpoint( Endpoint ), parseFile )

data Discovery = Static | StrictDns | LogicalDns

data Destination = Destination
  { discovery :: Discovery
  , hosts :: [(Text, Int)]
  , connectTimeout :: Duration
  , loadBalancer :: LoadBalancer
  , circuitBreaker :: Maybe CircuitBreaker
  , outlierDetection :: Maybe OutlierDetection
  , healthyPanicThreshold :: Maybe Int
  , tls :: Maybe Tls
  , tcpKeepalive :: Maybe TcpKeepalive
  , httpOptions :: HttpOptions
  , requestTimeout :: Maybe Duration
  , retryPolicy :: Maybe RetryPolicy }

data LoadBalancer = LeastRequest Int | Random | RoundRobin

data CircuitBreaker = CircuitBreaker
  { maxConnections :: Maybe Int
  , maxPendingRequests :: Maybe Int
  , maxRequests :: Maybe Int
  , maxRetries :: Maybe Int }

data OutlierDetection = OutlierDetection
  { interval :: Maybe Duration
  , baseEjectionTime :: Maybe Duration
  , maxEjectionPercent :: Maybe Int
  , consecutive5xx :: Maybe Consecutive
  , consecutiveGatewayFailure :: Maybe Consecutive
  , successRate :: Maybe SuccessRate
  , failurePercentage :: Maybe FailurePercentage
  , localOrigin :: Maybe LocalOrigin }

data Consecutive = Consecutive
  { enforcing :: Int
  , num :: Int }

data SuccessRate = SuccessRate
  { minimumHosts :: Int
  , requestVolume :: Int
  , stdevFactor :: Int
  , enforcing :: Int }

data FailurePercentage = FailurePercentage
  { minimumHosts :: Int
  , requestVolume :: Int
  , threshold :: Int
  , enforcing :: Int }

data LocalOrigin = LocalOrigin
  { consecutive :: Maybe Consecutive
  , enforcingSuccessRate :: Maybe Int
  , enforcingFailurePercentage :: Maybe Int }

data Tls = Tls { sni :: Maybe Text }

data TcpKeepalive = TcpKeepalive
  { probes :: Int
  , time :: Duration
  , interval :: Duration }

data HttpOptions = HttpOptions
  { idleTimeout :: Duration
  , httpVersion :: HttpVersion }

data HttpVersion = H1 | H2 | Downstream

data RetryPolicy = RetryPolicy
  { retriableStatusCodes :: [Int]
  , numRetries :: Int
  , perTryTimeout :: Maybe Duration
  , retryBackOff :: Maybe RetryBackOff }

data RetryBackOff = RetryBackOff
  { baseInterval :: Duration
  , maxInterval :: Duration }

data Listener r = Listener
  { host :: Text
  , port :: Int
  , http :: r }

data Routes
  = NoRoute
  | Dst Text
  | When { cond :: Condition, matched :: Routes, unmatched :: Routes }
  deriving Show

data Condition
  = Never
  | Always
  | Not Condition
  | And Condition Condition
  | Or Condition Condition
  | Match Part Matcher
  | OpenApi FilePath
  deriving Show

data Part = Header Text | Authority | Method | Path
  deriving Show

data Matcher = Exact Text | Prefix Text | Template [Maybe Text]
  deriving Show

data Rule = Rule
  { authority :: Maybe Matcher
  , method :: Maybe Matcher
  , path :: Maybe Matcher
  , headers :: [(Text, Matcher)]
  , action :: Maybe Text }

newtype RoutesException = RoutesException Part
  deriving Show

instance Exception RoutesException where

listenerRules :: Listener Routes -> IO (Listener [Rule])
listenerRules l@Listener{..} = fmap f (routingRules http)
  where
    f rs = l { http = rs }

routingRules :: Routes -> IO [Rule]
routingRules rs = do
  rs <- openApiRoutes rs
  rules <- traverse (foldRules emptyRule) (map (cnst . neg . cnd) (alt rs))
  pure (rules >>= id)
  where
    emptyRule :: Rule
    emptyRule = Rule Nothing Nothing Nothing [] Nothing

    openApiCondition :: Condition -> IO Condition
    openApiCondition (OpenApi fp) =
      let endpoint (Endpoint p m) = And (Match Method (Exact m)) (Match Path (Template p))
          f [] = Never
          f [e] = endpoint e
          f (e:es) = foldr (Or . endpoint) (endpoint e) es in
      fmap f (parseFile fp)
    openApiCondition c = pure c

    openApiRoutes :: Routes -> IO Routes
    openApiRoutes r@NoRoute = pure r
    openApiRoutes r@(Dst _) = pure r
    openApiRoutes (When c t e) = do
      c <- openApiCondition c
      t <- openApiRoutes t
      e <- openApiRoutes e
      pure (When c t e)

    foldRules :: Rule -> Routes -> IO [Rule]
    foldRules nr NoRoute | isJust (action nr) = error "[routingRules] NoRoute/Rule.action already set!"
    foldRules nr NoRoute = pure [nr]

    foldRules nr (Dst _) | isJust (action nr) = error "[routingRules] Dst/Rule.action already set!"
    foldRules nr (Dst t) = pure [nr { action = Just t }]

    foldRules nr (When (Match p@(Header n) _) _ _) | any (cmpCase n . fst) (headers nr) =
      throwIO (RoutesException p)
    foldRules nr (When (Match p@(Header n) m) t e) = do
      t <- foldRules (nr { headers = (n, m) : (headers nr) }) t
      e <- foldRules nr e
      pure (t ++ e)

    foldRules nr (When (Match p@Authority _) _ _) | isJust (authority nr) =
      throwIO (RoutesException p)
    foldRules nr (When (Match p@Authority m) t e) = do
      t <- foldRules (nr { authority = Just m }) t
      e <- foldRules nr e
      pure (t ++ e)

    foldRules nr (When (Match p@Method _) _ _) | isJust (method nr) = throwIO (RoutesException p)
    foldRules nr (When (Match p@Method m) t e) = do
      t <- foldRules (nr { method = Just m }) t
      e <- foldRules nr e
      pure (t ++ e)

    foldRules nr (When (Match p@Path _) _ _) | isJust (path nr) = throwIO (RoutesException p)
    foldRules nr (When (Match p@Path m) t e) = do
      t <- foldRules (nr { path = Just m }) t
      e <- foldRules nr e
      pure (t ++ e)

    foldRules nr (When c _ _) = error ("[routingRules] unexpected condition: " ++ show c)

    cmpCase a b = toCaseFold a == toCaseFold b

    alt NoRoute = [NoRoute]
    alt d@(Dst _) = [d]
    alt (When i t e) = do
      i <- alternatives i
      t <- alt t
      e <- alt e
      pure (When i t e)

    cnd NoRoute = NoRoute
    cnd d@(Dst _) = d
    cnd (When i t e) =
      cnd t & \t ->
      cnd e & \e ->
        foldr (\c a -> When c a e) t (conditions i)

    neg NoRoute = NoRoute
    neg d@(Dst _) = d
    neg (When i t e) =
      neg t & \t ->
      neg e & \e ->
      case simplifyNegation i of
        Not c -> When c e t
        _ -> When i t e

    cnst NoRoute = NoRoute
    cnst d@(Dst _) = d
    cnst (When Always e _) = e
    cnst (When Never _ e) = e
    cnst (When i t e) = When i (cnst t) (cnst e)

    -- eliminate Or constructor and returns possible alternatives
    alternatives :: Condition -> [Condition]
    alternatives c@(Match _ _) = [c]
    alternatives c@Never = [c]
    alternatives c@Always = [c]
    alternatives c@(OpenApi _) = [c]
    alternatives (Not c) = fmap Not (alternatives c)
    alternatives (And a b) = do
      a <- alternatives a
      b <- alternatives b
      pure (And a b)
    alternatives (Or a b) = alternatives a ++ alternatives b

    -- eliminate And constructor and returns contained conditions
    conditions :: Condition -> [Condition]
    conditions c@(Match _ _) = [c]
    conditions c@Never = [c]
    conditions c@Always = [c]
    conditions c@(OpenApi _) = [c]
    conditions (Not c) = fmap Not (conditions c)
    conditions (And a b) = conditions a ++ conditions b
    conditions (Or _ _) = error "[conditions] Condition.Or should be eliminated first"

    simplifyNegation :: Condition -> Condition
    simplifyNegation c@(Match _ _) = c
    simplifyNegation Never = Never
    simplifyNegation Always = Always
    simplifyNegation c@(OpenApi _) = c
    simplifyNegation (And a b) = And (simplifyNegation a) (simplifyNegation b)
    simplifyNegation (Or a b) = Or (simplifyNegation a) (simplifyNegation b)
    simplifyNegation (Not Always) = Never
    simplifyNegation (Not Never) = Always
    simplifyNegation (Not (Not c)) = simplifyNegation c
    simplifyNegation (Not c) = Not (simplifyNegation c)
