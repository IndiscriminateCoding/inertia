module Ast where

import Control.Exception( Exception )
import Control.Monad.Catch( MonadThrow(..) )
import Data.Function( (&) )
import Data.Maybe( isJust )
import Data.Text( Text, toCaseFold )

import Duration( Duration )

data Discovery = Static | StrictDns | LogicalDns
  deriving Show

data Destination = Destination
  { discovery :: Discovery
  , hosts :: [(Text, Int)]
  , connectTimeout :: Duration }
  deriving Show

data Listener r = Listener
  { host :: Text
  , port :: Int
  , http :: r }
  deriving Show

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
  deriving Show

data Part = Header Text | Authority | Method | Path
  deriving Show

data Matcher = Exact Text | Prefix Text
  deriving Show

data Rule = Rule
  { authority :: Maybe Matcher
  , method :: Maybe Matcher
  , path :: Maybe Matcher
  , headers :: [(Text, Matcher)]
  , action :: Maybe Text }
  deriving Show

newtype RoutesException = RoutesException Part
  deriving Show

instance Exception RoutesException where

listenerRules :: MonadThrow m => Listener Routes -> m (Listener [Rule])
listenerRules l@Listener{..} = fmap f (routingRules http)
  where
    f rs = l { http = rs }

routingRules :: MonadThrow m => Routes -> m [Rule]
routingRules = go emptyRule . cnst . neg . cnd . alt
  where
    emptyRule :: Rule
    emptyRule = Rule Nothing Nothing Nothing [] Nothing

    go :: MonadThrow m => Rule -> Routes -> m [Rule]
    go nr NoRoute | isJust (action nr) = error "[routingRules] NoRoute/Rule.action already set!"
    go nr NoRoute = pure [nr]

    go nr (Dst _) | isJust (action nr) = error "[routingRules] Dst/Rule.action already set!"
    go nr (Dst t) = pure [nr { action = Just t }]

    go nr (When (Match p@(Header n) _) _ _) | any (cmpCase n . fst) (headers nr) =
      throwM (RoutesException p)
    go nr (When (Match p@(Header n) m) t e) = do
      t <- go (nr { headers = (n, m) : (headers nr) }) t
      e <- go nr e
      pure (t ++ e)

    go nr (When (Match p@Authority _) _ _) | isJust (authority nr) = throwM (RoutesException p)
    go nr (When (Match p@Authority m) t e) = do
      t <- go (nr { authority = Just m }) t
      e <- go nr e
      pure (t ++ e)

    go nr (When (Match p@Method _) _ _) | isJust (method nr) = throwM (RoutesException p)
    go nr (When (Match p@Method m) t e) = do
      t <- go (nr { method = Just m }) t
      e <- go nr e
      pure (t ++ e)

    go nr (When (Match p@Path _) _ _) | isJust (path nr) = throwM (RoutesException p)
    go nr (When (Match p@Path m) t e) = do
      t <- go (nr { path = Just m }) t
      e <- go nr e
      pure (t ++ e)

    go nr (When c _ _) = error ("[routingRules] unexpected condition: " ++ show c)

    cmpCase a b = toCaseFold a == toCaseFold b

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
    conditions (Not c) = fmap Not (conditions c)
    conditions (And a b) = conditions a ++ conditions b
    conditions (Or _ _) = error "[conditions] Condition.Or should be eliminated first"

    simplifyNegation :: Condition -> Condition
    simplifyNegation c@(Match _ _) = c
    simplifyNegation Never = Never
    simplifyNegation Always = Always
    simplifyNegation (And a b) = And (simplifyNegation a) (simplifyNegation b)
    simplifyNegation (Or a b) = Or (simplifyNegation a) (simplifyNegation b)
    simplifyNegation (Not Always) = Never
    simplifyNegation (Not Never) = Always
    simplifyNegation (Not (Not c)) = simplifyNegation c
    simplifyNegation (Not c) = Not (simplifyNegation c)
