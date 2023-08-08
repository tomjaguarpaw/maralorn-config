module StatusScript.Env (Env (..)) where

import Maralorn.Prelude
import System.FSNotify qualified as Notify

data Env = MkEnv
  { fork :: Text -> IO () -> IO ()
  , watch_manager :: Notify.WatchManager
  , homeDir :: FilePath
  }
