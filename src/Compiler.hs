module Compiler where



import Dhall
-- import Dhall(InputSettings(..))

-- Module responsible for compiling the input dhall file and producing diagnostics
-- Only entire buffer(file) compilation is supported 
-- TODO: rename/move this module


-- | Default input settings: resolves imports relative to @.@ (the
-- current working directory), report errors as coming from @(input)@,
-- and default evaluation settings from 'defaultEvaluateSettings'.
--
inputSettings :: InputSettings
inputSettings = defaultInputSettings { _rootDirectory = "." }
    
--     InputSettings
--   { _rootDirectory = "."
--   , _sourceName = "(input)"
--   , _evaluateSettings = defaultEvaluateSettings
--   }


produceDiagnostics = inputExprWithSettings