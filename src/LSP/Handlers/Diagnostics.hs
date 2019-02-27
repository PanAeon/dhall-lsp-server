{-| This module contains everything related on how LSP server handles diagnostic messages. -}
module LSP.Handlers.Diagnostics( sendEmptyDiagnostics
                               , sendDiagnostics
                               ) where

import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Diagnostics
import qualified Language.Haskell.LSP.Control as LSP.Control
import qualified Language.Haskell.LSP.Core as LSP.Core
import qualified Language.Haskell.LSP.Types as LSP.Types
import qualified Language.Haskell.LSP.Utility  as LSP.Utility

import qualified Data.Aeson                            as J
import qualified Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J

import qualified System.IO.Unsafe
import qualified Data.Text.IO

import Backend.Dhall.Diagnostics

-- TODO: max num errors parameter (not rly relevant since we got 1, but still) 
-- ---------------------------------------------------------------------
-- Y no method to flush particular source errors?
sendEmptyDiagnostics ::  J.Uri -> Maybe Int -> ReaderT (LSP.Core.LspFuncs ()) IO ()
sendEmptyDiagnostics fileUri version =
  publishDiagnostics 10 fileUri version (partitionBySource [])

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: J.Uri -> Maybe Int ->  ReaderT (LSP.Core.LspFuncs ()) IO ()
sendDiagnostics fileUri version = do
  let
   filePath = maybe (error "can't convert uri to file path") id $ J.uriToFilePath fileUri -- !FIXME: handle non-file uris
  txt <- lift $ Data.Text.IO.readFile filePath
  -- pure $ System.IO.hPrint System.IO.stderr txt
  diags' <- lift $ compilerDiagnostics filePath (J.getUri fileUri) txt
  lift $ LSP.Utility.logs $ "diagnostic: " <> show diags'
  publishDiagnostics 10 fileUri version (partitionBySource diags')

  -- reactorSend $ J.NotificationMessage "2.0" "textDocument/publishDiagnostics" (Just r)
  -- publishDiagnostics 100 fileUri version (partitionBySource diags)

-- FIXME: why it doesn't send the correct version?
myGlobalVar :: IORef Int
{-# NOINLINE myGlobalVar #-}
myGlobalVar = System.IO.Unsafe.unsafePerformIO (newIORef 0)

-- FIXME: When published with the same version it will append to the list of diagnostic errors.
-- ! which is causing problems with save file, which doesn't provide version from client
-- * flushDiagnostics will flush all diagnostics, not only for the current file!
publishDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> ReaderT (LSP.Core.LspFuncs ()) IO ()
publishDiagnostics maxToPublish uri v diags = do
  lf <- ask
  v' <- readIORef myGlobalVar
  writeIORef myGlobalVar (v'+1)
  liftIO $ (LSP.Core.publishDiagnosticsFunc lf) maxToPublish uri (Just v') diags -- ! looks like a bug upstream 
  -- if Map.null diags
  --   then liftIO $ (Core.flushDiagnosticsBySourceFunc lf) maxToPublish (Just ("dhall-lsp-server"))
  --   else liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri v diags
  --Core.flushDiagnosticsBySourceFunc LspFuncs c Int Maybe DiagnosticSource
