module Main where

import Data.String.Interpolate (i)
import Relude
import Relude.Unsafe (fromJust)
import Shh (captureTrim, (|>))
import Shh qualified
import System.Directory.OsPath (listDirectory)
import System.OsPath (OsPath, decodeUtf, osp, (</>))

Shh.load Shh.Absolute ["mshow"]

mailDir, outBox, inBox :: OsPath
mailDir = [osp|/home/maralorn/Maildir/hera|]
inBox = mailDir </> [osp|Move/todo|]
outBox = mailDir </> [osp|Archiv/unsortiert|]

main :: IO ()
main = do
  let dirPath = inBox </> [osp|"cur"|]
  files <- listDirectory dirPath
  forM_ files \file -> do
    header :: Text <- decodeUtf8 <$> (mshow ["-q", fromJust . decodeUtf $ dirPath </> file] |> captureTrim)
    pure ()
