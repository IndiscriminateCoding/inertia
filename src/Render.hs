module Render where

import Data.Map.Strict( Map )
import Data.Text( Text )
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified Data.Map.Strict as Map

import Ast

changes :: Eq a => STM a -> IO (STM a)
changes s = do
  var <- newTVarIO Nothing
  pure $ do
    now <- s
    val <- readTVar var
    traverse (check . (now /=)) val
    writeTVar var (Just now)
    pure now

changesIO :: Eq a => STM a -> IO (IO a)
changesIO = fmap atomically . changes

data RenderCtx = RenderCtx
  { destinations :: TVar (Map Text Destination)
  , dnsHosts :: TVar [Text]
  , resolved :: TVar (Map Text [(Text, Int)])
  }

renderCtx :: Map Text Destination -> STM RenderCtx
renderCtx dsts = do
  destinations <- newTVar dsts
  dnsHosts <- newTVar []
  resolved <- newTVar Map.empty
  pure RenderCtx{..}

-- destinationsChanges :: RenderCtx -> IO (IO (Map Text Destination))
-- destinationsChanges RenderCtx{..} = changesIO (readTVar destinations)
