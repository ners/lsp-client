module Control.Concurrent.STM.TVar.Extra where

import Control.Concurrent.STM
import Prelude

overTVar :: (a -> a) -> TVar a -> STM a
overTVar f var = stateTVar var (\x -> (f x, f x))

overTVarIO :: (a -> a) -> TVar a -> IO a
overTVarIO = (atomically .) . overTVar

modifyTVarIO :: TVar a -> (a -> a) -> IO ()
modifyTVarIO = (atomically .) . modifyTVar

writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO = (atomically .) . writeTVar
