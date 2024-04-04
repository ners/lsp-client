{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}

module Language.LSP.Client.Session where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar.Extra
import Control.Exception (throw)
import Control.Lens hiding (Empty, List)
import Control.Lens.Extras (is)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.State (StateT, execState)
import Data.Default (def)
import Data.Foldable (foldl', foldr', forM_, toList)
import Data.Function (on)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (sortBy)
import Data.List.Extra (groupOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Row
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope.Mixed (Rope)
import Language.LSP.Client.Decoding
import Language.LSP.Client.Exceptions (SessionException (UnexpectedResponseError))
import Language.LSP.Protocol.Capabilities (fullCaps)
import Language.LSP.Protocol.Lens hiding (error, to)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import System.PosixCompat.Process (getProcessID)
import Language.LSP.VFS
    ( VFS
    , VfsLog
    , VirtualFile (..)
    , changeFromClientVFS
    , changeFromServerVFS
    , closeVFS
    , lsp_version
    , openVFS
    , vfsMap
    , virtualFileVersion
    )
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, (</>))
import System.FilePath.Glob qualified as Glob
import Prelude hiding (id)
import Prelude qualified

data SessionState = SessionState
    { initialized :: TMVar InitializeResult
    -- ^ The response of the initialization handshake, if any.
    , pendingRequests :: TVar RequestMap
    -- ^ Response callbacks for sent requests waiting for a response. Once a response arrives the request is removed from this map.
    , notificationHandlers :: TVar NotificationMap
    -- ^ Notification callbacks that fire whenever a notification of their type is received.
    , lastRequestId :: TVar Int32
    -- ^ A counter to send each request to the server is sent with a unique ID, allowing us to pair it back with its response.
    , serverCapabilities :: TVar (HashMap Text SomeRegistration)
    -- ^ The capabilities that the server has dynamically registered with us so far.
    , clientCapabilities :: ClientCapabilities
    -- ^ The client capabilities advertised to the server. Not a `TVar` because it does not change during the session.
    , progressTokens :: TVar (HashSet ProgressToken)
    -- ^ Progress messages received from the server.
    , outgoing :: TQueue FromClientMessage
    -- ^ Messages that have been serialised but not yet written to the output handle.
    , vfs :: TVar VFS
    -- ^ Virtual, in-memory file system of the files known to the LSP.
    , rootDir :: FilePath
    -- ^ The root of the project as sent to the server. Document URIs are relative to it. Not a `TVar` because it does not change during the session.
    }

defaultSessionState :: VFS -> IO SessionState
defaultSessionState vfs' = do
    initialized <- newEmptyTMVarIO
    pendingRequests <- newTVarIO emptyRequestMap
    notificationHandlers <- newTVarIO emptyNotificationMap
    lastRequestId <- newTVarIO 0
    serverCapabilities <- newTVarIO mempty
    progressTokens <- newTVarIO mempty
    outgoing <- newTQueueIO
    vfs <- newTVarIO vfs'
    pure
        SessionState
            { rootDir = "."
            , clientCapabilities = def
            , ..
            }

{- | A session representing one instance of launching and connecting to a server.
It is essentially an STM-backed `StateT`: despite it being `ReaderT`, it can still
mutate `TVar` values.
-}
type Session = ReaderT SessionState IO

documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri

-- eitherOf :: APrism' s a -> (a -> b) -> (s -> b) -> s -> b
-- eitherOf p a b = either b a . matching p
--
-- anyOf :: [APrism' s a] -> (a -> b) -> b -> s -> b
-- anyOf [] _ b = const b
-- anyOf (p : prisms) a b = eitherOf p a $ anyOf prisms a b

{- | Fires whenever the client receives a message from the server. Updates the session state as needed.
Note that this does not provide any business logic beyond updating the session state; you most likely
want to use `sendRequest` and `receiveNotification` to register callbacks for specific messages.
-}
handleServerMessage :: FromServerMessage -> Session ()
handleServerMessage (FromServerMess SMethod_Progress req) =
    when (anyOf folded ($ req ^. params . value) [is _workDoneProgressBegin, is _workDoneProgressEnd])
        $ asks progressTokens
        >>= liftIO
        . flip modifyTVarIO (HashSet.insert $ req ^. params . token)
handleServerMessage (FromServerMess SMethod_ClientRegisterCapability req) =
    asks serverCapabilities >>= liftIO . flip modifyTVarIO (HashMap.union (HashMap.fromList newRegs))
  where
    regs = req ^.. params . registrations . traversed . to toSomeRegistration . _Just
    newRegs = (\sr@(SomeRegistration r) -> (r ^. id, sr)) <$> regs
handleServerMessage (FromServerMess SMethod_ClientUnregisterCapability req) =
    asks serverCapabilities >>= liftIO . flip modifyTVarIO (flip (foldr' HashMap.delete) unRegs)
  where
    unRegs = (^. id) <$> req ^. params . unregisterations
handleServerMessage (FromServerMess SMethod_WorkspaceApplyEdit r) = do
    -- First, prefer the versioned documentChanges field
    allChangeParams <- case r ^. params . edit . documentChanges of
        Just cs -> do
            mapM_ (checkIfNeedsOpened . documentChangeUri) cs
            -- replace the user provided version numbers with the VFS ones + 1
            -- (technically we should check that the user versions match the VFS ones)
            cs' <- traverseOf (traverse . _L . textDocument) bumpNewestVersion cs
            return $ mapMaybe getParamsFromDocumentChange cs'
        -- Then fall back to the changes field
        Nothing -> case r ^. params . edit . changes of
            Just cs -> do
                mapM_ checkIfNeedsOpened (Map.keys cs)
                concat <$> mapM (uncurry getChangeParams) (Map.toList cs)
            Nothing ->
                error "WorkspaceEdit contains neither documentChanges nor changes!"

    asks vfs >>= liftIO . flip modifyTVarIO (execState $ changeFromServerVFS logger r)

    let groupedParams = groupOn (view textDocument) allChangeParams
        mergedParams = mergeParams <$> groupedParams

    forM_ mergedParams $ sendNotification SMethod_TextDocumentDidChange

    -- Update VFS to new document versions
    let sortedVersions = sortBy (compare `on` (^. textDocument . version)) <$> groupedParams
        latestVersions = view textDocument . last <$> sortedVersions

    forM_ latestVersions $ \(VersionedTextDocumentIdentifier uri v) ->
        asks vfs
            >>= liftIO
            . flip
                modifyTVarIO
                ( \vfs -> do
                    let update (VirtualFile _ file_ver t) = VirtualFile v (file_ver + 1) t
                     in vfs & vfsMap . ix (toNormalizedUri uri) %~ update
                )
    sendResponse
        r
        $ Right
            ApplyWorkspaceEditResult
                { _applied = True
                , _failureReason = Nothing
                , _failedChange = Nothing
                }
  where
    logger :: LogAction (StateT VFS Identity) (WithSeverity VfsLog)
    logger = LogAction $ \(WithSeverity msg sev) -> case sev of Error -> error $ show msg; _ -> pure ()
    checkIfNeedsOpened uri = do
        isOpen <- asks vfs >>= liftIO . readTVarIO <&> has (vfsMap . ix (toNormalizedUri uri))

        -- if its not open, open it
        unless isOpen $ do
            contents <- maybe (pure "") (liftIO . Text.readFile) (uriToFilePath uri)
            sendNotification
                SMethod_TextDocumentDidOpen
                DidOpenTextDocumentParams
                    { _textDocument =
                        TextDocumentItem
                            { _uri = uri
                            , _languageId = ""
                            , _version = 0
                            , _text = contents
                            }
                    }

    getParamsFromTextDocumentEdit :: TextDocumentEdit -> Maybe DidChangeTextDocumentParams
    getParamsFromTextDocumentEdit (TextDocumentEdit docId edits) = do
        DidChangeTextDocumentParams <$> docId ^? _versionedTextDocumentIdentifier <*> pure (editToChangeEvent <$> edits)

    editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
    editToChangeEvent (InR e) = TextDocumentContentChangeEvent $ InL $ #range .== (e ^. range) .+ #rangeLength .== Nothing .+ #text .== (e ^. newText)
    editToChangeEvent (InL e) = TextDocumentContentChangeEvent $ InL $ #range .== (e ^. range) .+ #rangeLength .== Nothing .+ #text .== (e ^. newText)

    getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
    getParamsFromDocumentChange (InL textDocumentEdit) = getParamsFromTextDocumentEdit textDocumentEdit
    getParamsFromDocumentChange _ = Nothing

    bumpNewestVersion :: OptionalVersionedTextDocumentIdentifier -> Session OptionalVersionedTextDocumentIdentifier
    bumpNewestVersion (OptionalVersionedTextDocumentIdentifier uri (InL _)) = do
        nextVersion <- head <$> textDocumentVersions uri
        pure $ OptionalVersionedTextDocumentIdentifier uri $ InL nextVersion._version
    bumpNewestVersion i = pure i

    -- For a uri returns an infinite list of versions [n,n+1,n+2,...]
    -- where n is the current version
    textDocumentVersions :: Uri -> Session [VersionedTextDocumentIdentifier]
    textDocumentVersions uri = do
        vfs <- asks vfs >>= liftIO . readTVarIO
        let curVer = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . lsp_version
        pure $ VersionedTextDocumentIdentifier uri <$> [curVer + 1 ..]

    textDocumentEdits uri edits = do
        vers <- textDocumentVersions uri
        pure $ zipWith (\v e -> TextDocumentEdit (review _versionedTextDocumentIdentifier v) [InL e]) vers edits

    getChangeParams uri edits = do
        edits <- textDocumentEdits uri (reverse edits)
        pure $ mapMaybe getParamsFromTextDocumentEdit edits

    mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
    mergeParams params =
        let events = concat $ toList $ toList . (^. contentChanges) <$> params
         in DidChangeTextDocumentParams (head params ^. textDocument) events
handleServerMessage (FromServerMess SMethod_WindowWorkDoneProgressCreate req) = sendResponse req $ Right Null
handleServerMessage _ = pure ()

{- | Sends a request to the server, with a callback that fires when the response arrives.
Multiple requests can be waiting at the same time.
-}
sendRequest
    :: forall (m :: Method 'ClientToServer 'Request)
     . (TMessage m ~ TRequestMessage m)
    => SMethod m
    -> MessageParams m
    -> (TResponseMessage m -> IO ())
    -> Session (LspId m)
sendRequest requestMethod params requestCallback = do
    reqId <- asks lastRequestId >>= liftIO . overTVarIO (+ 1) <&> IdInt
    asks pendingRequests >>= liftIO . flip modifyTVarIO (updateRequestMap reqId RequestCallback{..})
    sendMessage $ fromClientReq $ TRequestMessage "2.0" reqId requestMethod params
    pure reqId

{- | Send a response to the server. This is used internally to acknowledge server requests.
Users of this library cannot register callbacks to server requests, so this function is probably of no use to them.
-}
sendResponse
    :: forall (m :: Method 'ServerToClient 'Request)
     . TRequestMessage m
    -> Either ResponseError (MessageResult m)
    -> Session ()
sendResponse req res = do
    sendMessage $ FromClientRsp req._method $ TResponseMessage req._jsonrpc (Just req._id) res

-- | Sends a request to the server and synchronously waits for its response.
request
    :: forall (m :: Method 'ClientToServer 'Request)
     . (TMessage m ~ TRequestMessage m)
    => SMethod m
    -> MessageParams m
    -> Session (TResponseMessage m)
request method params = do
    done <- liftIO newEmptyMVar
    void $ sendRequest method params $ putMVar done
    liftIO $ takeMVar done

{- | Checks the response for errors and throws an exception if needed.
 Returns the result if successful.
-}
getResponseResult :: TResponseMessage m -> MessageResult m
getResponseResult response = either err Prelude.id $ response ^. result
  where
    lid = SomeLspId $ fromJust response._id
    err = throw . UnexpectedResponseError lid

-- | Sends a notification to the server. Updates the VFS if the notification is a document update.
sendNotification
    :: forall (m :: Method 'ClientToServer 'Notification)
     . (TMessage m ~ TNotificationMessage m)
    => SMethod m
    -> MessageParams m
    -> Session ()
sendNotification m params = do
    let n = TNotificationMessage "2.0" m params
    vfs <- asks vfs
    case m of
        SMethod_TextDocumentDidOpen -> liftIO $ modifyTVarIO vfs (execState $ openVFS mempty n)
        SMethod_TextDocumentDidClose -> liftIO $ modifyTVarIO vfs (execState $ closeVFS mempty n)
        SMethod_TextDocumentDidChange -> liftIO $ modifyTVarIO vfs (execState $ changeFromClientVFS mempty n)
        _ -> pure ()
    sendMessage $ fromClientNot n

{- | Registers a callback for notifications received from the server.
If multiple callbacks are registered for the same notification method, they will all be called.
-}
receiveNotification
    :: forall (m :: Method 'ServerToClient 'Notification)
     . (TMessage m ~ TNotificationMessage m)
    => SMethod m
    -> (TMessage m -> IO ())
    -> Session ()
receiveNotification method notificationCallback =
    asks notificationHandlers
        >>= liftIO
        . flip
            modifyTVarIO
            ( appendNotificationCallback method NotificationCallback{..}
            )

{- | Clears the registered callback for the given notification method, if any.
If multiple callbacks have been registered, this clears /all/ of them.
-}
clearNotificationCallback
    :: forall (m :: Method 'ServerToClient 'Notification)
     . SMethod m
    -> Session ()
clearNotificationCallback method =
    asks notificationHandlers
        >>= liftIO
        . flip
            modifyTVarIO
            ( removeNotificationCallback method
            )

-- | Queues a message to be sent to the server at the client's earliest convenience.
sendMessage :: FromClientMessage -> Session ()
sendMessage msg = asks outgoing >>= liftIO . atomically . (`writeTQueue` msg)

lspClientInfo :: Rec ("name" .== Text .+ "version" .== Maybe Text)
lspClientInfo = #name .== "lsp-client" .+ #version .== Just CURRENT_PACKAGE_VERSION

{- | Performs the initialisation handshake and synchronously waits for its completion.
When the function completes, the session is initialised.
-}
initialize :: Session ()
initialize = do
    pid <- liftIO getProcessID
    response <-
        request
            SMethod_Initialize
            InitializeParams
                { _workDoneToken = Nothing
                , _processId = InL $ fromIntegral pid
                , _clientInfo = Just lspClientInfo
                , _locale = Nothing
                , _rootPath = Nothing
                , _rootUri = InR Null
                , _initializationOptions = Nothing
                , _capabilities = fullCaps
                , _trace = Just TraceValues_Off
                , _workspaceFolders = Nothing
                }
    asks initialized >>= liftIO . atomically . flip putTMVar (getResponseResult response)
    sendNotification SMethod_Initialized InitializedParams

{- | /Creates/ a new text document. This is different from 'openDoc'
 as it sends a @workspace/didChangeWatchedFiles@ notification letting the server
 know that a file was created within the workspace, __provided that the server
 has registered for it__, and the file matches any patterns the server
 registered for.
 It /does not/ actually create a file on disk, but is useful for convincing
 the server that one does exist.
-}
createDoc
    :: FilePath
    -- ^ The path to the document to open, __relative to the root directory__.
    -> Text
    -- ^ The text document's language identifier, e.g. @"haskell"@.
    -> Text
    -- ^ The content of the text document to create.
    -> Session TextDocumentIdentifier
    -- ^ The identifier of the document just created.
createDoc file language contents = do
    serverCaps <- asks serverCapabilities >>= liftIO . readTVarIO
    clientCaps <- asks clientCapabilities
    rootDir <- asks rootDir
    absFile <- liftIO $ canonicalizePath (rootDir </> file)
    let pred :: SomeRegistration -> [TRegistration 'Method_WorkspaceDidChangeWatchedFiles]
        pred (SomeRegistration r@TRegistration{_method = SMethod_WorkspaceDidChangeWatchedFiles}) = [r]
        pred _ = mempty
        regs :: [TRegistration 'Method_WorkspaceDidChangeWatchedFiles]
        regs = concatMap pred $ HashMap.elems serverCaps
        watchHits :: FileSystemWatcher -> Bool
        watchHits (FileSystemWatcher (GlobPattern (InL (Pattern pattern))) kind) =
            fileMatches (Text.unpack pattern) && maybe True containsCreate kind
        watchHits _ = False

        fileMatches :: String -> Bool
        fileMatches pattern = Glob.match (Glob.compile pattern) (if isAbsolute pattern then absFile else file)

        regHits :: TRegistration 'Method_WorkspaceDidChangeWatchedFiles -> Bool
        regHits reg = any watchHits $ reg ^. registerOptions . _Just . watchers

        clientCapsSupports =
            clientCaps
                ^? workspace
                    . _Just
                    . didChangeWatchedFiles
                    . _Just
                    . dynamicRegistration
                    . _Just
                    == Just True
        shouldSend = clientCapsSupports && foldl' (\acc r -> acc || regHits r) False regs

    when shouldSend
        $ sendNotification SMethod_WorkspaceDidChangeWatchedFiles
        $ DidChangeWatchedFilesParams
            [FileEvent (filePathToUri $ rootDir </> file) FileChangeType_Created]
    openDoc' file language contents

{- | Opens a text document that /exists on disk/, and sends a
 @textDocument/didOpen@ notification to the server.
-}
openDoc :: FilePath -> Text -> Session TextDocumentIdentifier
openDoc file language = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    contents <- liftIO $ Text.readFile fp
    openDoc' file language contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: FilePath -> Text -> Text -> Session TextDocumentIdentifier
openDoc' file language contents = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
        uri = filePathToUri fp
        item = TextDocumentItem uri language 0 contents
    sendNotification SMethod_TextDocumentDidOpen (DidOpenTextDocumentParams item)
    pure $ TextDocumentIdentifier uri

-- | Closes a text document and sends a @textDocument/didClose@ notification to the server.
closeDoc :: TextDocumentIdentifier -> Session ()
closeDoc docId = do
    let params = DidCloseTextDocumentParams (TextDocumentIdentifier (docId ^. uri))
    sendNotification SMethod_TextDocumentDidClose params

-- | Changes a text document and sends a @textDocument/didChange@ notification to the server.
changeDoc :: TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> Session ()
changeDoc docId changes = do
    verDoc <- getVersionedDoc docId
    let params = DidChangeTextDocumentParams (verDoc & version +~ 1) changes
    sendNotification SMethod_TextDocumentDidChange params

-- | Gets the Uri for the file relative to the session's root directory.
getDocUri :: FilePath -> Session Uri
getDocUri file = do
    rootDir <- asks rootDir
    let fp = rootDir </> file
    return $ filePathToUri fp

-- | The current text contents of a document.
documentContents :: TextDocumentIdentifier -> Session (Maybe Rope)
documentContents (TextDocumentIdentifier uri) = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    pure $ vfs ^? vfsMap . ix (toNormalizedUri uri) . to _file_text

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: TextDocumentIdentifier -> Session VersionedTextDocumentIdentifier
getVersionedDoc (TextDocumentIdentifier uri) = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    let ver = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri uri) . to virtualFileVersion
    pure $ VersionedTextDocumentIdentifier uri ver
