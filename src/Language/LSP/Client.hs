{- |
This module provides utilities to run an LSP `Session` in `IO`.
-}
module Language.LSP.Client where

import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (asks, runReaderT)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Dependent.Map qualified as DMap
import Data.Either (fromLeft)
import Data.Generics.Labels ()
import Language.LSP.Client.Decoding
import Language.LSP.Client.Encoding (encode)
import Language.LSP.Client.Session
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.VFS (emptyVFS)
import System.IO (Handle)
import UnliftIO (concurrently_, race)
import Prelude

{- | Starts a new session, using the specified handles to communicate with the
server.
-}
runSessionWithHandles
    :: Handle
    -- ^ The input handle: messages sent from the server to the client will be read from here
    -> Handle
    -- ^ The output handle: messages sent by the client will be written here
    -> Session a
    -- ^ Session actions
    -> IO a
runSessionWithHandles input output action = do
    initialState <- defaultSessionState emptyVFS
    flip runReaderT initialState $ do
        actionResult <- race action $ do
            let send = do
                    message <- asks outgoing >>= liftIO . atomically . readTQueue
                    liftIO $ LazyByteString.hPut output $ encode message
            let receive = do
                    serverBytes <- liftIO $ getNextMessage input
                    (serverMessage, requestCallback) <-
                        asks pendingRequests
                            >>= liftIO
                                . atomically
                                . flip stateTVar (decodeFromServerMsg serverBytes)
                    handleServerMessage serverMessage
                    liftIO requestCallback
                    case serverMessage of
                        LSP.FromServerMess smethod msg -> case LSP.splitServerMethod smethod of
                            LSP.IsServerNot -> do
                                handlers :: NotificationMap <- asks notificationHandlers >>= liftIO . readTVarIO
                                let NotificationCallback cb = DMap.findWithDefault (NotificationCallback (const $ pure ())) smethod handlers
                                liftIO $ cb msg
                            _ -> pure ()
                        _ -> pure ()
            concurrently_ (forever send) (forever receive)
        pure $ fromLeft (error "runSessionWithHandle: send/receive thread should not exit") actionResult
