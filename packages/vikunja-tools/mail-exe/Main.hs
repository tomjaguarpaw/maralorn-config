module Main where

import Data.Aeson (ToJSON, encode)
import Data.String.Interpolate (i)
import Data.Text (stripPrefix)
import Data.Text qualified as Text
import Network.Wreq (putWith)
import Relude
import Relude.Unsafe (fromJust)
import System.Directory.OsPath (listDirectory)
import System.OsPath (OsPath, decodeUtf, osp, (</>))
import System.Process.Typed (byteStringInput, proc, readProcessStdout_, runProcess_, setStdin)
import System.Which (staticWhich)
import Vikunja

mailDir, outBox, inBox :: OsPath
mailDir = [osp|/home/maralorn/Maildir/hera|]
inBox = mailDir </> [osp|Move/todo|]
outBox = mailDir </> [osp|Archiv/unsortiert|]

unsafeToString :: OsPath -> FilePath
unsafeToString = fromJust . decodeUtf

data NewTask = MkNewTask
  { title :: Text
  , description :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

mshow :: [String] -> IO Text
mshow args = fmap (Text.strip . toStrict . decodeUtf8) . readProcessStdout_ $ proc $(staticWhich "mshow") args

main :: IO ()
main = do
  let dirPath = inBox </> [osp|cur|]
  opts <- defaultOptions
  files <- listDirectory dirPath
  forM_ files \filename -> do
    let filepath = unsafeToString $ dirPath </> filename
    header <- lines <$> mshow ["-h", "from:subject", "-q", filepath]
    let from = maybe "" (Text.strip . Text.takeWhile (/= '<')) (stripPrefix "From: " =<< viaNonEmpty head header)
        subject = fromMaybe "" $ stripPrefix "Subject: " =<< viaNonEmpty head (drop 1 header)
        title = [i|Mail: #{from}: #{subject}|] :: Text
    msg <- mshow ["-h", "from:subject:to:cc:date:message-id", filepath]
    putTextLn [i|Creating task #{title}|]
    _ <-
      putWith
        opts
        [i|#{url}/projects/#{defaultProject}/tasks|]
        (encode $ MkNewTask title (Text.intercalate "\n<br>\n" $ lines msg))
    runProcess_ $
      setStdin (byteStringInput (encodeUtf8 filepath)) $
        proc $(staticWhich "mrefile") [unsafeToString outBox]
