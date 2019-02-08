module Compiler where





import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.Context
import qualified Dhall.Core
import Dhall.Core (Expr(..), Chunks(..))
import Dhall.Parser (Src(..))
import Dhall.Import(Imported(..))
import qualified Dhall.Binary
import Dhall.Binary(StandardVersion)
import Dhall.TypeCheck (DetailedTypeError(..), TypeError, X)
import qualified Dhall.TypeCheck


import qualified Control.Monad.Trans.State.Strict as State 
import qualified Control.Exception
import Lens.Family (LensLike', set, view)
import qualified System.Exit
import qualified System.IO
-- import Dhall(InputSettings(..))

-- Module responsible for compiling the input dhall file and producing diagnostics
-- Only entire buffer(file) compilation is supported 
-- TODO: rename/move this module


-- | Default input settings: resolves imports relative to @.@ (the
-- current working directory), report errors as coming from @(input)@,
-- and default evaluation settings from 'defaultEvaluateSettings'.

    
--     InputSettings
--   { _rootDirectory = "."
--   , _sourceName = "(input)"
--   , _evaluateSettings = defaultEvaluateSettings
--   }

------------- have to copy from dhall a few functions to make setting's accessors available

throws :: Exception e => Either e a -> IO a
throws (Left  e) = Control.Exception.throwIO e
throws (Right r) = return r

-- | Access the directory to resolve imports relative to.
--
-- @since 1.16
rootDirectory
  :: (Functor f)
  => LensLike' f InputSettings FilePath
rootDirectory k s =
  fmap (\x -> s { _rootDirectory = x }) (k (_rootDirectory s))

-- | Access the name of the source to report locations from; this is
-- only used in error messages, so it's okay if this is a best guess
-- or something symbolic.
--
-- @since 1.16
sourceName
  :: (Functor f)
  => LensLike' f InputSettings FilePath
sourceName k s =
  fmap (\x -> s { _sourceName = x}) (k (_sourceName s))


-- | @since 1.16
data EvaluateSettings = EvaluateSettings
  { _startingContext :: Dhall.Context.Context (Expr Src X)
  , _normalizer      :: Dhall.Core.ReifiedNormalizer X
  , _standardVersion :: StandardVersion
  }

startingContext
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Dhall.Context.Context (Expr Src X))
startingContext = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Dhall.Context.Context (Expr Src X))
    l k s = fmap (\x -> s { _startingContext = x}) (k (_startingContext s))

-- | Access the custom normalizer.
--
-- @since 1.16
normalizer
  :: (Functor f, HasEvaluateSettings s)
  => LensLike' f s (Dhall.Core.ReifiedNormalizer X)
normalizer = evaluateSettings . l
  where
    l :: (Functor f)
      => LensLike' f EvaluateSettings (Dhall.Core.ReifiedNormalizer X)
    l k s = fmap (\x -> s { _normalizer = x }) (k (_normalizer s))

-- | Access the standard version (used primarily when encoding or decoding
-- Dhall expressions to and from a binary representation)
--
-- @since 1.17
standardVersion
    :: (Functor f, HasEvaluateSettings s)
    => LensLike' f s StandardVersion
standardVersion = evaluateSettings . l
  where
  l k s = fmap (\x -> s { _standardVersion = x}) (k (_standardVersion s))

-- | @since 1.16
class HasEvaluateSettings s where
  evaluateSettings
    :: (Functor f)
    => LensLike' f s EvaluateSettings

instance HasEvaluateSettings InputSettings where
  evaluateSettings k s =
    fmap (\x -> s { _evaluateSettings = x }) (k (_evaluateSettings s))

instance HasEvaluateSettings EvaluateSettings where
  evaluateSettings = id
-- | Default evaluation settings: no extra entries in the initial
-- context, and no special normalizer behaviour.
--
-- @since 1.16
defaultEvaluateSettings :: EvaluateSettings
defaultEvaluateSettings = EvaluateSettings
  { _startingContext = Dhall.Context.empty
  , _normalizer      = Dhall.Core.ReifiedNormalizer (const (pure Nothing))
  , _standardVersion = Dhall.Binary.defaultStandardVersion
  }

-- | @since 1.16
data InputSettings = InputSettings
  { _rootDirectory :: FilePath
  , _sourceName :: FilePath
  , _evaluateSettings :: EvaluateSettings
  }

-- | Default input settings: resolves imports relative to @.@ (the
-- current working directory), report errors as coming from @(input)@,
-- and default evaluation settings from 'defaultEvaluateSettings'.
--
-- @since 1.16
defaultInputSettings :: InputSettings
defaultInputSettings = InputSettings
  { _rootDirectory = "."
  , _sourceName = "(input)"
  , _evaluateSettings = defaultEvaluateSettings
  } 

inputExprWithSettings
    :: InputSettings
    -> Text
    -- ^ The Dhall program
    -> IO (Expr Src X)
    -- ^ The fully normalized AST
inputExprWithSettings settings txt = do
    expr  <- throws (Dhall.Parser.exprFromText (view sourceName settings) txt)

    let InputSettings {..} = settings 

    let EvaluateSettings {..} = _evaluateSettings

    let transform =
               set Dhall.Import.standardVersion _standardVersion
            .  set Dhall.Import.normalizer      _normalizer
            .  set Dhall.Import.startingContext _startingContext

    let status = transform (Dhall.Import.emptyStatus _rootDirectory)

    expr' <- State.evalStateT (Dhall.Import.loadWith expr) status

    _ <- throws (Dhall.TypeCheck.typeWith (view startingContext settings) expr')
    pure (Dhall.Core.normalizeWith (Dhall.Core.getReifiedNormalizer (view normalizer settings)) expr')
 
-- * TODO: imported must be handled either at file level, or much better at import location
compilerDiagnostics :: Text -> IO ()
compilerDiagnostics txt = undefined
  where
    settings =  ( set rootDirectory "." 
                . set sourceName "<buffer_name>") defaultInputSettings
    ast = inputExprWithSettings  settings txt
    handle =   Control.Exception.handle allErrors
             . Control.Exception.handle importErrors
             . Control.Exception.handle fileErrors


    allErrors e = do
      let _ = e :: SomeException
      System.IO.hPrint System.IO.stderr e
      System.Exit.exitFailure
    importErrors (Imported ps e) = do
      let _ = e :: TypeError Src X
      pure 5
    fileErrors e = do
      let _ = e :: TypeError Src X
      pure 3

{-
need: {start l:c} -- {end}, can get from sourcePos (megaparsec) from Src
message, and related info


data Diagnostic

Constructors
Diagnostic	 

    _range :: Range 
    _severity :: Maybe DiagnosticSeverity 
    _code :: Maybe Text 
    _source :: Maybe DiagnosticSource 
    _message :: Text 
    _relatedInformation :: Maybe (List DiagnosticRelatedInformation) 

let handle =
                Control.Exception.handle handler2
            .   Control.Exception.handle handler1
            .   Control.Exception.handle handler0
          where
            handler0 e = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (DetailedTypeError e)
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO e

            handler1 (Imported ps e) = do
                let _ = e :: TypeError Src X
                System.IO.hPutStrLn System.IO.stderr ""
                if explain
                    then Control.Exception.throwIO (Imported ps (DetailedTypeError e))
                    else do
                        Data.Text.IO.hPutStrLn System.IO.stderr "\ESC[2mUse \"dhall --explain\" for detailed errors\ESC[0m"
                        Control.Exception.throwIO (Imported ps e)

            handler2 e = do
                let _ = e :: SomeException
                System.IO.hPrint System.IO.stderr e
                System.Exit.exitFailure
-}




produceDiagnostics = inputExprWithSettings