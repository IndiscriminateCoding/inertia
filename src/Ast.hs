module Ast where

import Data.Function( (&) )
import Data.List( find )
import Data.Maybe( isJust, maybe )
import Data.Text( Text )

import qualified Data.Text as T

import Duration( Duration )
import OpenApi( Endpoint( Endpoint ), parseFile )
import Re

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

data Matcher = Exact Text | Prefix Text | Template Re
  deriving Show

mergeMatchers :: Matcher -> Matcher -> Maybe Matcher
mergeMatchers e@(Exact a) (Exact b) | a == b = Just e
mergeMatchers (Exact _) (Exact _) = Nothing
mergeMatchers (Prefix a) p@(Prefix b) | a `T.isPrefixOf` b = Just p
mergeMatchers p@(Prefix a) (Prefix b) | b `T.isPrefixOf` a = Just p
mergeMatchers (Prefix _) (Prefix _) = Nothing
mergeMatchers x@(Exact e) (Prefix p) | p `T.isPrefixOf` e = Just x
mergeMatchers (Prefix p) x@(Exact e) | p `T.isPrefixOf` e = Just x
mergeMatchers (Exact _) (Prefix _) = Nothing
mergeMatchers (Prefix _) (Exact _) = Nothing
mergeMatchers (Template t) m = mergeWithTemplate t m
mergeMatchers m (Template t) = mergeWithTemplate t m

matcherRegex :: Matcher -> Re
matcherRegex (Exact t) = literal t
matcherRegex (Prefix t) = T.foldr Chr (Any Eps) t
matcherRegex (Template r) = r

mergeWithTemplate :: Re -> Matcher -> Maybe Matcher
mergeWithTemplate r m = fmap Template (merge r (matcherRegex m))

data Rule = Rule
  { authority :: Maybe Matcher
  , method :: Maybe Matcher
  , path :: Maybe Matcher
  , headers :: [(Text, Matcher)]
  , action :: Maybe Text }

listenerRules :: Listener Routes -> IO (Listener [Rule])
listenerRules l@Listener{..} = fmap f (routingRules http)
  where
    f rs = l { http = rs }

routingRules :: Routes -> IO [Rule]
routingRules rs = openApiRoutes rs >>= foldRules emptyRule . cnst . neg . cnd . alt
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
    openApiCondition (Not c) = fmap Not (openApiCondition c)
    openApiCondition (Or a b) = do
      a <- openApiCondition a
      b <- openApiCondition b
      pure (Or a b)
    openApiCondition (And a b) = do
      a <- openApiCondition a
      b <- openApiCondition b
      pure (And a b)
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
    foldRules nr NoRoute | isJust (action nr) =
      error "[routingRules] NoRoute/Rule.action already set!"
    foldRules nr NoRoute = pure [nr]

    foldRules nr (Dst _) | isJust (action nr) = error "[routingRules] Dst/Rule.action already set!"
    foldRules nr (Dst t) = pure [nr { action = Just t }]

    foldRules nr (When (Match (Header n) m) t e) =
      let hdrs = filter (not . cmpCase n . fst) (headers nr)
          added =
            case find (cmpCase n . fst) (headers nr) of
              Nothing -> Just (n, m)
              Just (_, m') -> fmap (n,) (mergeMatchers m m') in
      case added of
        Nothing -> pure []
        Just added -> do
          t <- foldRules (nr { headers = added : hdrs }) t
          e <- foldRules nr e
          pure (t ++ e)

    foldRules nr (When (Match p@Authority m) t e) =
      let new = maybe (Just m) (mergeMatchers m) (authority nr) in
      case new of
        Nothing -> pure []
        Just new -> do
          t <- foldRules (nr { authority = Just new }) t
          e <- foldRules nr e
          pure (t ++ e)

    foldRules nr (When (Match p@Method m) t e) =
      let new = maybe (Just m) (mergeMatchers m) (method nr) in
      case new of
        Nothing -> pure []
        Just new -> do
          t <- foldRules (nr { method = Just new }) t
          e <- foldRules nr e
          pure (t ++ e)

    foldRules nr (When (Match p@Path m) t e) =
      let new = maybe (Just m) (mergeMatchers m) (path nr) in
      case new of
        Nothing -> pure []
        Just new -> do
          t <- foldRules (nr { path = Just new }) t
          e <- foldRules nr e
          pure (t ++ e)

    foldRules nr (When c _ _) = error ("[routingRules] unexpected condition: " ++ show c)

    cmpCase a b = T.toCaseFold a == T.toCaseFold b

    alt NoRoute = NoRoute
    alt d@(Dst _) = d
    alt (When i t e) =
      alt t & \t ->
      alt e & \e ->
        foldr (\c a -> When c t a) e (alternatives i)

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
