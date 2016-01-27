module RETypes where

import System.Plugins.Load
import System.Plugins.Make
import Data.Maybe
import System.Directory
import System.FileLock
import Data.Map
import Control.Monad.Trans.State

data ReState = ReState Module (Map String String)
type ParamType = String
type ErrorCode = Int
type Callback = String -> [ParamType] -> IO (ErrorCode, [ParamType])
type RE a = StateT (Callback, ReState) IO a
type RuleType = [ParamType] -> RE (ErrorCode, [ParamType])
