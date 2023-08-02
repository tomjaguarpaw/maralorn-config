module StatusScript.CommandUtil (tryCmd, reportMissing) where

import Data.ByteString.Lazy qualified as LBS
import Maralorn.Prelude
import Shh ((|>))
import Shh qualified

tryCmd :: Shh.Proc a -> IO LBS.ByteString
tryCmd x = Shh.ignoreFailure x |> Shh.captureTrim

reportMissing :: MonadIO m => IO [FilePath] -> m ()
reportMissing missing = whenJustM (nonEmpty <$> liftIO missing) \missing' -> sayErr [i|missing executables #{missing'}|]
