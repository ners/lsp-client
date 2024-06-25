{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Language.LSP.VFS
import System.Directory (canonicalizePath)
import System.FilePath (isAbsolute, (</>))
import System.FilePath.Glob qualified as Glob
import System.PosixCompat.Process (getProcessID)
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

defaultSessionState :: (MonadIO io) => VFS -> io SessionState
defaultSessionState vfs' = liftIO $ do
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
type SessionT = ReaderT SessionState

type Session = SessionT IO

documentChangeUri :: DocumentChange -> Uri
documentChangeUri (InL x) = x ^. textDocument . uri
documentChangeUri (InR (InL x)) = x ^. uri
documentChangeUri (InR (InR (InL x))) = x ^. oldUri
documentChangeUri (InR (InR (InR x))) = x ^. uri

{- | Fires whenever the client receives a message from the server. Updates the session state as needed.
Note that this does not provide any business logic beyond updating the session state; you most likely
want to use `sendRequest` and `receiveNotification` to register callbacks for specific messages.
-}
handleServerMessage :: forall io. (MonadIO io) => FromServerMessage -> SessionT io ()
handleServerMessage (FromServerMess SMethod_Progress req) =
    when (anyOf folded ($ req ^. params . value) [is _workDoneProgressBegin, is _workDoneProgressEnd]) $
        asks progressTokens
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

    forM_ latestVersions $ \VersionedTextDocumentIdentifier{..} ->
        asks vfs
            >>= liftIO
                . flip
                    modifyTVarIO
                    ( vfsMap . ix (toNormalizedUri _uri) %~ ((lsp_version .~ _version) . (file_version +~ 1))
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
    logger = LogAction $ \WithSeverity{..} -> case getSeverity of Error -> error $ show getMsg; _ -> pure ()

    checkIfNeedsOpened :: Uri -> SessionT io ()
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
        _textDocument <- docId ^? _versionedTextDocumentIdentifier
        let _contentChanges = editToChangeEvent <$> edits
        pure DidChangeTextDocumentParams{..}

    editToChangeEvent :: TextEdit |? AnnotatedTextEdit -> TextDocumentContentChangeEvent
    editToChangeEvent (InR e) = TextDocumentContentChangeEvent $ InL $ #range .== (e ^. range) .+ #rangeLength .== Nothing .+ #text .== (e ^. newText)
    editToChangeEvent (InL e) = TextDocumentContentChangeEvent $ InL $ #range .== (e ^. range) .+ #rangeLength .== Nothing .+ #text .== (e ^. newText)

    getParamsFromDocumentChange :: DocumentChange -> Maybe DidChangeTextDocumentParams
    getParamsFromDocumentChange (InL textDocumentEdit) = getParamsFromTextDocumentEdit textDocumentEdit
    getParamsFromDocumentChange _ = Nothing

    bumpNewestVersion :: OptionalVersionedTextDocumentIdentifier -> SessionT io OptionalVersionedTextDocumentIdentifier
    bumpNewestVersion OptionalVersionedTextDocumentIdentifier{_uri, _version = InL _} = do
        VersionedTextDocumentIdentifier{_version} <- head <$> textDocumentVersions _uri
        pure OptionalVersionedTextDocumentIdentifier{_version = InL _version, ..}
    bumpNewestVersion i = pure i

    -- For a uri returns an infinite list of versions [n+1,n+2,...]
    -- where n is the current version
    textDocumentVersions :: Uri -> SessionT io [VersionedTextDocumentIdentifier]
    textDocumentVersions _uri = do
        tail . iterate (version +~ 1) <$> getVersionedDoc TextDocumentIdentifier{_uri}

    textDocumentEdits :: Uri -> [TextEdit] -> SessionT io [TextDocumentEdit]
    textDocumentEdits uri edits = do
        versions <- textDocumentVersions uri
        pure $
            zipWith
                ( \v e ->
                    TextDocumentEdit
                        { _edits = [InL e]
                        , _textDocument = review _versionedTextDocumentIdentifier v
                        }
                )
                versions
                edits

    getChangeParams uri edits = do
        edits <- textDocumentEdits uri (reverse edits)
        pure $ mapMaybe getParamsFromTextDocumentEdit edits

    mergeParams :: [DidChangeTextDocumentParams] -> DidChangeTextDocumentParams
    mergeParams params =
        DidChangeTextDocumentParams
            { _contentChanges = concat . toList $ toList . (^. contentChanges) <$> params
            , _textDocument = head params ^. textDocument
            }
handleServerMessage (FromServerMess SMethod_WindowWorkDoneProgressCreate req) = sendResponse req $ Right Null
handleServerMessage _ = pure ()

{- | Sends a request to the server, with a callback that fires when the response arrives.
Multiple requests can be waiting at the same time.
-}
sendRequest
    :: forall (m :: Method 'ClientToServer 'Request) io
     . (TMessage m ~ TRequestMessage m, MonadIO io)
    => SMethod m
    -> MessageParams m
    -> (TResponseMessage m -> IO ())
    -> SessionT io (LspId m)
sendRequest requestMethod _params requestCallback = do
    _id <- asks lastRequestId >>= liftIO . overTVarIO (+ 1) <&> IdInt
    asks pendingRequests >>= liftIO . flip modifyTVarIO (updateRequestMap _id RequestCallback{..})
    sendMessage $ fromClientReq TRequestMessage{_jsonrpc = "2.0", _method = requestMethod, ..}
    pure _id

{- | Send a response to the server. This is used internally to acknowledge server requests.
Users of this library cannot register callbacks to server requests, so this function is probably of no use to them.
-}
sendResponse
    :: forall (m :: Method 'ServerToClient 'Request) io
     . (MonadIO io)
    => TRequestMessage m
    -> Either ResponseError (MessageResult m)
    -> SessionT io ()
sendResponse TRequestMessage{..} _result =
    sendMessage $ FromClientRsp _method TResponseMessage{_id = Just _id, ..}

-- | Sends a request to the server and synchronously waits for its response.
request
    :: forall (m :: Method 'ClientToServer 'Request) io
     . (TMessage m ~ TRequestMessage m, MonadIO io)
    => SMethod m
    -> MessageParams m
    -> SessionT io (TResponseMessage m)
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
    :: forall (m :: Method 'ClientToServer 'Notification) io
     . (TMessage m ~ TNotificationMessage m, MonadIO io)
    => SMethod m
    -> MessageParams m
    -> SessionT io ()
sendNotification m params = do
    let n = TNotificationMessage "2.0" m params
    vfs <- asks vfs
    case m of
        SMethod_TextDocumentDidOpen -> liftIO . modifyTVarIO vfs . execState $ openVFS mempty n
        SMethod_TextDocumentDidClose -> liftIO . modifyTVarIO vfs . execState $ closeVFS mempty n
        SMethod_TextDocumentDidChange -> liftIO . modifyTVarIO vfs . execState $ changeFromClientVFS mempty n
        _ -> pure ()
    sendMessage $ fromClientNot n

{- | Registers a callback for notifications received from the server.
If multiple callbacks are registered for the same notification method, they will all be called.
-}
receiveNotification
    :: forall (m :: Method 'ServerToClient 'Notification) io
     . (TMessage m ~ TNotificationMessage m, MonadIO io)
    => SMethod m
    -> (TMessage m -> IO ())
    -> SessionT io ()
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
    :: forall (m :: Method 'ServerToClient 'Notification) io
     . (MonadIO io)
    => SMethod m
    -> SessionT io ()
clearNotificationCallback method =
    asks notificationHandlers
        >>= liftIO
            . flip
                modifyTVarIO
                ( removeNotificationCallback method
                )

-- | Queues a message to be sent to the server at the client's earliest convenience.
sendMessage :: (MonadIO io) => FromClientMessage -> SessionT io ()
sendMessage msg = asks outgoing >>= liftIO . atomically . (`writeTQueue` msg)

lspClientInfo :: Rec ("name" .== Text .+ "version" .== Maybe Text)
lspClientInfo = #name .== "lsp-client" .+ #version .== Just CURRENT_PACKAGE_VERSION

{- | Performs the initialisation handshake and synchronously waits for its completion.
When the function completes, the session is initialised.
-}
initialize :: (MonadIO io) => SessionT io ()
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
    :: (MonadIO io)
    => FilePath
    -- ^ The path to the document to open, __relative to the root directory__.
    -> Text
    -- ^ The text document's language identifier, e.g. @"haskell"@.
    -> Text
    -- ^ The content of the text document to create.
    -> SessionT io TextDocumentIdentifier
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
        watchHits FileSystemWatcher{_globPattern = GlobPattern (InL (Pattern pattern)), _kind} =
            fileMatches (Text.unpack pattern) && maybe True containsCreate _kind
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

    when shouldSend $
        sendNotification
            SMethod_WorkspaceDidChangeWatchedFiles
            DidChangeWatchedFilesParams
                { _changes =
                    [ FileEvent
                        { _type_ = FileChangeType_Created
                        , _uri = filePathToUri $ rootDir </> file
                        }
                    ]
                }
    openDoc' file language contents

{- | Opens a text document that /exists on disk/, and sends a
 @textDocument/didOpen@ notification to the server.
-}
openDoc :: (MonadIO io) => FilePath -> Text -> SessionT io TextDocumentIdentifier
openDoc file language = do
    rootDir <- asks rootDir
    contents <- liftIO . Text.readFile $ rootDir </> file
    openDoc' file language contents

{- | This is a variant of `openDoc` that takes the file content as an argument.
 Use this is the file exists /outside/ of the current workspace.
-}
openDoc' :: (MonadIO io) => FilePath -> Text -> Text -> SessionT io TextDocumentIdentifier
openDoc' file language contents = do
    rootDir <- asks rootDir
    let _uri = filePathToUri $ rootDir </> file
    sendNotification
        SMethod_TextDocumentDidOpen
        DidOpenTextDocumentParams
            { _textDocument =
                TextDocumentItem
                    { _text = contents
                    , _languageId = language
                    , _version = 0
                    , _uri
                    }
            }
    pure TextDocumentIdentifier{..}

-- | Closes a text document and sends a @textDocument/didClose@ notification to the server.
closeDoc :: (MonadIO io) => TextDocumentIdentifier -> SessionT io ()
closeDoc docId =
    sendNotification
        SMethod_TextDocumentDidClose
        DidCloseTextDocumentParams
            { _textDocument =
                TextDocumentIdentifier
                    { _uri = docId ^. uri
                    }
            }

-- | Changes a text document and sends a @textDocument/didChange@ notification to the server.
changeDoc :: (MonadIO io) => TextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> SessionT io ()
changeDoc docId _contentChanges = do
    _textDocument <- getVersionedDoc docId <&> version +~ 1
    sendNotification SMethod_TextDocumentDidChange DidChangeTextDocumentParams{..}

-- | Gets the Uri for the file relative to the session's root directory.
getDocUri :: (MonadIO io) => FilePath -> SessionT io Uri
getDocUri file = do
    rootDir <- asks rootDir
    pure . filePathToUri $ rootDir </> file

-- | The current text contents of a document.
documentContents :: (MonadIO io) => TextDocumentIdentifier -> SessionT io (Maybe Rope)
documentContents TextDocumentIdentifier{_uri} = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    pure $ vfs ^? vfsMap . ix (toNormalizedUri _uri) . to _file_text

-- | Adds the current version to the document, as tracked by the session.
getVersionedDoc :: (MonadIO io) => TextDocumentIdentifier -> SessionT io VersionedTextDocumentIdentifier
getVersionedDoc TextDocumentIdentifier{_uri} = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    let _version = fromMaybe 0 $ vfs ^? vfsMap . ix (toNormalizedUri _uri) . to virtualFileVersion
    pure VersionedTextDocumentIdentifier{..}

-- | Get all the versioned documents tracked by the session.
getAllVersionedDocs :: (MonadIO io) => SessionT io [VersionedTextDocumentIdentifier]
getAllVersionedDocs = do
    vfs <- asks vfs >>= liftIO . readTVarIO
    pure $
        Map.toList (vfs ^. vfsMap) <&> \(nuri, vf) ->
            VersionedTextDocumentIdentifier
                { _uri = fromNormalizedUri nuri
                , _version = virtualFileVersion vf
                }
