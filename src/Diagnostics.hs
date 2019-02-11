{-# LANGUAGE RankNTypes            #-}
module Diagnostics(compilerDiagnostics) where


{-|
This module is responsible for producing compiler diagnostic (errors, warns, etc ...)
-}

import qualified Control.Exception
import qualified Dhall
import Dhall(rootDirectory, sourceName, defaultInputSettings, inputExprWithSettings)
import Dhall.Parser(ParseError(..), Src(..))
import qualified Dhall.Core
import qualified System.Exit
import qualified System.IO
import Lens.Family (LensLike', set, view)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError(..), X)
import Dhall.Import(Imported(..))
import qualified Data.Text as T


import qualified Text.Megaparsec
import qualified Text.Megaparsec.Error
import Text.Show(ShowS)
import qualified Data.Set

import Language.Haskell.LSP.Types(
      Diagnostic(..)
    , Range(..)
    , DiagnosticSeverity(..)
    , DiagnosticSource(..)
    , DiagnosticRelatedInformation(..)
    , Position(..)
    )



-- import Text.Megaparsec.Error

exampleDoc :: Text
exampleDoc = "{ foo = 1"
           <> ", bar = [3.0, 4.0, 5.0, []]"
           <> "}"

defaultDiagnosticSource :: DiagnosticSource
defaultDiagnosticSource = "dhall-lsp-server"

-- FIXME: type errors span across whitespace after the expression
-- * TODO: `imported` errors must be handled either as a file error, or much better at import location
compilerDiagnostics :: Text -> Text -> IO [Diagnostic]
compilerDiagnostics filePath txt = handle ast
  where
    bufferName = T.unpack $ last $ fromList $ T.split (=='/') filePath
    settings =  ( set rootDirectory "." 
                . set sourceName bufferName) defaultInputSettings
    ast =  [] <$ inputExprWithSettings  settings txt
    handle =   Control.Exception.handle allErrors
             . Control.Exception.handle parseErrors
             . Control.Exception.handle importErrors
             . Control.Exception.handle moduleErrors
             
    
    allErrors e = do -- FIXME: somehow imported doesn't work now!
      let _ = e :: SomeException
          numLines = length $ T.lines txt
      pure [Diagnostic {
              _range = Range (Position 0 0) (Position numLines 0)
            , _severity = Just DsError
            , _source = Just defaultDiagnosticSource
            , _code = Nothing
            , _message = "Internal error has occurred: " <> (show e)
            , _relatedInformation = Nothing
            }]
    parseErrors e = do
        let _ = e :: ParseError
            bundle = unwrap e
        -- System.IO.hPrint System.IO.stderr e
        pure [Diagnostic {
              _range = Range (Position 0 0) (Position 3 0)
            , _severity = Just DsError
            , _source = Just defaultDiagnosticSource
            , _code = Nothing
            , _message = "Parse error: " <> (show e)
            , _relatedInformation = Nothing
            }]
    importErrors (Imported ps e) = do
      let _ = e :: TypeError Src X
      System.IO.hPrint System.IO.stderr (show ps)
      pure [ Diagnostic {
               _range = getSourceRange e
             , _severity = Just DsError
             , _source = Just defaultDiagnosticSource
             , _code = Nothing
             , _message =  ("import error: " <> (show e)) -- FIXME: looks like this is a bit wrong
             , _relatedInformation = Nothing
             }]
    moduleErrors e = do
      let _ = e :: TypeError Src X
      -- System.IO.hPrint System.IO.stderr txt    
      -- System.IO.hPrint System.IO.stderr e
      pure [ Diagnostic {
        _range = getSourceRange e
      , _severity = Just DsError
      , _source = Just defaultDiagnosticSource
      , _code = Nothing
      , _message =  (show e) -- FIXME: looks like this is a bit wrong
      , _relatedInformation = Nothing
      }] 

getSourceRange :: TypeError Src X -> Range
getSourceRange (TypeError ctx expr msg) =  case expr of
                Dhall.Core.Note (Src (Text.Megaparsec.SourcePos _ bl bc) (Text.Megaparsec.SourcePos _ el ec) _) _ -> 
                  Range (Position (unPos bl - 1) (unPos bc - 1)) (Position (unPos el - 1) (unPos ec - 1))
                _        -> error  "expected note" -- $ Range (Position 0 0) (Position (negate 1) 0) -- FIXME: default case 
  where
    unPos = Text.Megaparsec.unPos
-- Megaparsec utils:

-- FIXME: add propper error reporting for parse errors
getSourceRangeFromBundle 
   :: forall s e. ( Text.Megaparsec.Stream s
                 , Text.Megaparsec.Error.ShowErrorComponent e
                 )
  => Text.Megaparsec.ParseErrorBundle s e -- ^ Parse error bundle to display
  -> Range
getSourceRangeFromBundle Text.Megaparsec.Error.ParseErrorBundle {..} = undefined

 

errorBundleTextPretty
  :: forall s e. ( Text.Megaparsec.Stream s
                 , Text.Megaparsec.Error.ShowErrorComponent e
                 )
  => Text.Megaparsec.ParseErrorBundle s e -- ^ Parse error bundle to display
  -> String               -- ^ Textual rendition of the bundle
errorBundleTextPretty Text.Megaparsec.Error.ParseErrorBundle {..} = undefined
   

-- errorBundlePretty
--   :: forall s e. ( Text.Megaparsec.Stream s
--                  , Text.Megaparsec.Error.ShowErrorComponent e
--                  )
--   => Text.Megaparsec.ParseErrorBundle s e -- ^ Parse error bundle to display
--   -> String               -- ^ Textual rendition of the bundle
-- errorBundlePretty Text.Megaparsec.Error.ParseErrorBundle {..} =
--   let (r, _) = foldl' f (id, bundlePosState) bundleErrors
--   in drop 1 (r "")
--   where
--     f :: (ShowS, Text.Megaparsec.PosState s)
--       -> Text.Megaparsec.ParseError s e
--       -> (ShowS, Text.Megaparsec.PosState s)
--     f (o, !pst) e = (o . (outChunk ++), pst')
--       where
--         (epos, sline, pst') = reachOffset (errorOffset e) pst
--         outChunk =
--           "\n" <> sourcePosPretty epos <> ":\n" <>
--           padding <> "|\n" <>
--           lineNumber <> " | " <> sline <> "\n" <>
--           padding <> "| " <> rpadding <> pointer <> "\n" <>
--           Text.Megaparsec.Error.parseErrorTextPretty e
--         lineNumber = (show . unPos . sourceLine) epos
--         padding = replicate (length lineNumber + 1) ' '
--         rpadding =
--           if pointerLen > 0
--             then replicate rpshift ' '
--             else ""
--         rpshift = unPos (sourceColumn epos) - 1
--         pointer = replicate pointerLen '^'
--         pointerLen =
--           if rpshift + elen > slineLen
--             then slineLen - rpshift + 1
--             else elen
--         slineLen = length sline
--         elen =
--           case e of
--             Text.Megaparsec.TrivialError _ Nothing _ -> 1
--             Text.Megaparsec.TrivialError _ (Just x) _ -> errorItemLength x
--             Text.Megaparsec.FancyError _ xs ->
--               Data.Set.foldl' (\a b -> max a (errorFancyLength b)) 1 xs
 