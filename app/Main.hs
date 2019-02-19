{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent(forkIO)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                     as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
-- import           Control.Monad.STM
import qualified Data.Aeson                            as J
import           Data.Default
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import qualified Language.Haskell.LSP.Control          as CTRL
import qualified Language.Haskell.LSP.Core             as Core
import           Language.Haskell.LSP.Diagnostics
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import qualified Language.Haskell.LSP.Utility          as U
import           Language.Haskell.LSP.VFS
import qualified System.Exit
import qualified System.Log.Logger                     as L
import qualified Yi.Rope                               as Yi

import qualified Diagnostics
import qualified Data.Text.IO
import qualified System.IO
import qualified Data.Map as Map
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative

import qualified System.IO.Unsafe

data Options = Options {
  command :: Command
}

data Command = CmdVersion | Default 

parseOptions :: Parser Options
parseOptions = Options <$> parseMode


subcommand :: String -> String -> Parser a -> Parser a
subcommand name description parser =
    Options.Applicative.hsubparser
        (   Options.Applicative.command name parserInfo
        <>  Options.Applicative.metavar name
        )
  where
    parserInfo =
        Options.Applicative.info parser
            (   Options.Applicative.fullDesc
            <>  Options.Applicative.progDesc description
            )

parseMode :: Parser Command
parseMode =
        subcommand
            "version"
            "Display version"
            (pure CmdVersion)
    <|> pure Default
  
parserInfoOptions :: ParserInfo Options
parserInfoOptions =
    Options.Applicative.info
        (Options.Applicative.helper <*> parseOptions)
        (   Options.Applicative.progDesc "Interpreter for the Dhall language"
        <>  Options.Applicative.fullDesc
        )
        
runCommand :: Options -> IO ()
runCommand Options{..} = case command of
  CmdVersion -> putStrLn "0.0.1-SNAPSHOT"
  Default    ->  
   run (pure ()) >>= \case
    0 -> exitSuccess
    c -> exitWith . System.Exit.ExitFailure $ c

main :: IO ()
main = Options.Applicative.execParser parserInfoOptions >>= runCommand
  

-- ---------------------------------------------------------------------

run :: IO () -> IO Int
run dispatcherProc = flip E.catches handlers $ do

  rin  <- atomically newTChan :: IO (TChan ReactorInput)

  let
    dp lf = do
      liftIO $ U.logs "main.run:dp entered"
      _rpid  <- forkIO $ reactor lf rin
      liftIO $ U.logs "main.run:dp tchan"
      dispatcherProc
      liftIO $ U.logs "main.run:dp after dispatcherProc"
      return Nothing

  flip E.finally finalProc $ do
    Core.setupLogger (Just "/tmp/lsp-hello.log") [] L.DEBUG
    CTRL.run (return (Right ()), dp) (lspHandlers rin) lspOptions (Just "/tmp/lsp-hello-session.log")

  where
    handlers = [ E.Handler ioExcept
               , E.Handler someExcept
               ]
    finalProc = L.removeAllHandlers
    ioExcept   (e :: E.IOException)       = error $ show $ e -- print e >> return 1
    someExcept (e :: E.SomeException)     = error $ show $ e -- print e >> return 1

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

data ReactorInput
  = HandlerRequest FromClientMessage
      -- ^ injected into the reactor input by each of the individual callback handlers

-- ---------------------------------------------------------------------

-- | The monad used in the reactor
type R c a = ReaderT (Core.LspFuncs c) IO a

-- ---------------------------------------------------------------------
-- reactor monad functions
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------

reactorSend :: FromServerMessage -> R () ()
reactorSend msg = do
  lf <- ask
  liftIO $ Core.sendFunc lf msg

-- ---------------------------------------------------------------------
myGlobalVar :: IORef Int
{-# NOINLINE myGlobalVar #-}
myGlobalVar = System.IO.Unsafe.unsafePerformIO (newIORef 17)

-- ! NOW This will flush all diagnostics, not only for the current file!
-- ! SO it's version mismatch..?
publishDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> R () ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  v' <- readIORef myGlobalVar
  writeIORef myGlobalVar (v'+1)
  liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri (Just v') diags
  -- if Map.null diags
  --   then liftIO $ (Core.flushDiagnosticsBySourceFunc lf) maxToPublish (Just ("dhall-lsp-server"))
  --   else liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri v diags
  

-- ---------------------------------------------------------------------

nextLspReqId :: R () J.LspId
nextLspReqId = do
  f <- asks Core.getNextReqId
  liftIO f

-- ---------------------------------------------------------------------

-- | The single point that all events flow through, allowing management of state
-- to stitch replies and requests together from the two asynchronous sides: lsp
-- server and backend compiler
reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
reactor lf inp = do
  liftIO $ U.logs "reactor:entered"
  flip runReaderT lf $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of

      -- Handle any response from a message originating at the server, such as
      -- "workspace/applyEdit"
      HandlerRequest (RspFromClient rm) -> do
        liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

      HandlerRequest (NotInitialized _notification) -> do
        liftIO $ U.logm "****** reactor: processing Initialized Notification"
        -- Server is ready, register any specific capabilities we need

         {-
         Example:
         {
                 "method": "client/registerCapability",
                 "params": {
                         "registrations": [
                                 {
                                         "id": "79eee87c-c409-4664-8102-e03263673f6f",
                                         "method": "textDocument/willSaveWaitUntil",
                                         "registerOptions": {
                                                 "documentSelector": [
                                                         { "language": "javascript" }
                                                 ]
                                         }
                                 }
                         ]
                 }
         }
        -}
        let
          registration = J.Registration "dhall-lsp-server-registered" J.WorkspaceExecuteCommand Nothing
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

        -- example of showMessageRequest
        -- let
        --   params = J.ShowMessageRequestParams J.MtWarning "choose an option for XXX"
        --                    (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
        -- rid1 <- nextLspReqId

        -- reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 params

      -- -------------------------------

      HandlerRequest (NotDidOpenTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            v    = notification ^. J.params
                                 . J.textDocument 
                                 . J.version
            fileName =  J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc (Just v)

      -- -------------------------------

      -- HandlerRequest (NotDidChangeTextDocument notification) -> do
      --   let doc :: J.Uri
      --       doc  = notification ^. J.params
      --                            . J.textDocument
      --                            . J.uri
      --   mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
      --   case mdoc of
      --     Just (VirtualFile _version str) -> 
      --       liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: vf got:" ++ (show $ Yi.toString str)
      --     Nothing -> 
      --       liftIO $ U.logs "reactor:processing NotDidChangeTextDocument: vf returned Nothing"

      --   liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++ (show doc)

      -- -------------------------------

      HandlerRequest (NotDidSaveTextDocument notification) -> do
        liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc Nothing

      -- -------------------------------

      HandlerRequest (ReqRename req) -> do
        liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
        let
            _params = req ^. J.params
            _doc  = _params ^. J.textDocument . J.uri
            J.Position _l _c' = _params ^. J.position
            _newName  = _params ^. J.newName

        let we = J.WorkspaceEdit
                    Nothing -- "changes" field is deprecated
                    (Just (J.List [])) -- populate with actual changes from the rename
        let rspMsg = Core.makeResponseMessage req we
        reactorSend $ RspRename rspMsg

      -- -------------------------------

      -- HandlerRequest (ReqHover req) -> do
      --   liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
      --   let J.TextDocumentPositionParams _doc pos = req ^. J.params
      --       J.Position _l _c' = pos

      --   let
      --     ht = Just $ J.Hover ms (Just range)
      --     ms = J.List [J.CodeString $ J.LanguageString "lsp-hello" "TYPE INFO" ]
      --     range = J.Range pos pos
      --   reactorSend $ RspHover $ Core.makeResponseMessage req ht

      -- -------------------------------

      -- HandlerRequest (ReqCodeAction req) -> do
      --   liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
      --   let params = req ^. J.params
      --       doc = params ^. J.textDocument
      --       -- fileName = drop (length ("file://"::String)) doc
      --       -- J.Range from to = J._range (params :: J.CodeActionParams)
      --       (J.List diags) = params ^. J.context . J.diagnostics

      --   let
      --     -- makeCommand only generates commands for diagnostics whose source is us
      --     makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _l) = [J.Command title cmd cmdparams]
      --       where
      --         title = "Apply LSP hello command:" <> (head $ fromList (T.lines _m))
      --         -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
      --         cmd = "lsp-hello-command"
      --         -- need 'file' and 'start_pos'
      --         args = J.List
      --                 [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
      --                 , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
      --                 ]
      --         cmdparams = Just args
      --     makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
      --   let body = J.List $ map J.CACommand $ concatMap makeCommand diags
      --       rsp = Core.makeResponseMessage req body
      --   reactorSend $ RspCodeAction rsp

      -- -------------------------------

      HandlerRequest (ReqExecuteCommand req) -> do
        liftIO $ U.logs "reactor:got ExecuteCommandRequest:" -- ++ show req
        let params = req ^. J.params
            margs = params ^. J.arguments

        liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs

        let
          reply v = reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req v
        -- When we get a RefactorResult or HieDiff, we need to send a
        -- separate WorkspaceEdit Notification
          r = J.List [] :: J.List Int
        liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
        case toWorkspaceEdit r of
          Just we -> do
            reply (J.Object mempty)
            lid <- nextLspReqId
            -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
            reactorSend $ ReqApplyWorkspaceEdit $ fmServerApplyWorkspaceEditRequest lid we
          Nothing ->
            reply (J.Object mempty)

      -- -------------------------------
      HandlerRequest (NotDidCloseTextDocument req) -> do
        liftIO $ U.logm "****** reactor: processing NotDidCloseTextDocument"
        let
            doc  = req ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ U.logs $ "********* fileName=" ++ show fileName
        sendEmptyDiagnostics doc Nothing
      HandlerRequest om -> do
        liftIO $ U.logs $ "\nIGNORING!!!\n HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------

toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
toWorkspaceEdit _ = Nothing

-- ---------------------------------------------------------------------
-- Y no method to flush particular source errors?
sendEmptyDiagnostics ::  J.Uri -> Maybe Int -> R () ()
sendEmptyDiagnostics fileUri version = 
  publishDiagnostics 10 fileUri version (partitionBySource [])

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Uri -> Maybe Int -> R () ()
sendDiagnostics fileUri version = do
  let
   filePath = maybe (error "can't convert uri to file path") id $ J.uriToFilePath fileUri
  txt <- lift $ Data.Text.IO.readFile filePath
  -- pure $ System.IO.hPrint System.IO.stderr txt
  diags' <- lift $ Diagnostics.compilerDiagnostics filePath (J.getUri fileUri) txt 
  lift $ U.logs $ "diagnostic: " <> show diags'
  publishDiagnostics 10 fileUri version (partitionBySource diags')
  -- let  
  --   diags = [J.Diagnostic
  --             (J.Range (J.Position 0 1) (J.Position 0 5))
  --             (Just J.DsWarning)  -- severity
  --             Nothing  -- code
  --             (Just "lsp-hello") -- source
  --             "Example diagnostic message"
  --             (Just (J.List []))
  --           ]
    
  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  -- publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
  { J._openClose         = Just True
  , J._change            = Just J.TdSyncNone--J.TdSyncIncremental
  , J._willSave          = Just False
  , J._willSaveWaitUntil = Just False
  , J._save              = Just $ J.SaveOptions $ Just False
  }

lspOptions :: Core.Options
lspOptions = def { Core.textDocumentSync = Just syncOptions
                 , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
                 }

lspHandlers :: TChan ReactorInput -> Core.Handlers
lspHandlers rin
  = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
        , Core.renameHandler                            = Just $ passHandler rin ReqRename
        , Core.hoverHandler                             = Just $ passHandler rin ReqHover
        , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
        , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
        , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
        , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
        , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
        , Core.responseHandler                          = Just $ responseHandlerCb rin
        , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
        , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
        }

-- ---------------------------------------------------------------------

passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
passHandler rin c notification = do
  atomically $ writeTChan rin (HandlerRequest (c notification))

-- ---------------------------------------------------------------------

responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
responseHandlerCb _rin resp = do
  U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp

-- ---------------------------------------------------------------------



