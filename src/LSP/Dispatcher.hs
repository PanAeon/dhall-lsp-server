module LSP.Dispatcher(dispatcher) where

import           Control.Concurrent.STM.TChan
import           Language.Haskell.LSP.Messages
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Language.Haskell.LSP.Utility  as LSP.Utility

import qualified Data.Aeson                            as J
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J

import LSP.Common
import LSP.Handlers.Diagnostics

import Control.Lens 

-- ! FIXME: logs/logm
-- | A basic router, which reads from Client messages queue `inp` and executes appropriate actions
dispatcher :: LSP.Core.LspFuncs () -> TChan FromClientMessage -> IO ()
dispatcher lf inp = do
  liftIO $ LSP.Utility.logs "inside dispatcher"
  flip runReaderT lf $ forever $ do
    inval <- liftIO $ atomically $ readTChan inp
    case inval of

       (RspFromClient rm) ->
        liftIO $ LSP.Utility.logs $ "reactor:got RspFromClient:" ++ show rm

      -- -------------------------------

       (NotInitialized _notification) -> do
        liftIO $ LSP.Utility.logm "****** reactor: processing Initialized Notification"
        
        let
          registration = J.Registration "dhall-lsp-server-registered" J.WorkspaceExecuteCommand Nothing
        let registrations = J.RegistrationParams (J.List [registration])
        rid <- nextLspReqId

        -- client/registerCapability 
        sendToClient $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations

        -- example of showMessageRequest
        -- let
        --   params = J.ShowMessageRequestParams J.MtWarning "choose an option for XXX"
        --                    (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
        -- rid1 <- nextLspReqId

        -- reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 params

      -- -------------------------------

       (NotDidOpenTextDocument notification) -> do
        liftIO $ LSP.Utility.logm "****** reactor: processing NotDidOpenTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri
            v    = notification ^. J.params
                                 . J.textDocument
                                 . J.version
            fileName =  J.uriToFilePath doc
        liftIO $ LSP.Utility.logs $ "********* fileName=" <> show fileName <> "version: " <> show v
        sendDiagnostics doc (Just v)

      

       (NotDidSaveTextDocument notification) -> do
        liftIO $ LSP.Utility.logm "****** reactor: processing NotDidSaveTextDocument"
        let
            doc  = notification ^. J.params
                                 . J.textDocument
                                 . J.uri

            fileName = J.uriToFilePath doc
        liftIO $ LSP.Utility.logs $ "********* fileName=" ++ show fileName
        sendDiagnostics doc Nothing

      
       (NotDidCloseTextDocument req) -> do
        liftIO $ LSP.Utility.logm "****** reactor: processing NotDidCloseTextDocument"
        let
            doc  = req ^. J.params
                                 . J.textDocument
                                 . J.uri
            fileName = J.uriToFilePath doc
        liftIO $ LSP.Utility.logs $ "********* fileName=" ++ show fileName
        sendEmptyDiagnostics doc Nothing
       om -> do
        liftIO $ LSP.Utility.logs $ "\nIGNORING!!!\n HandlerRequest:" ++ show om

-- ---------------------------------------------------------------------