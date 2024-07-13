module Bluefin.Dialog.ReflexDom.TH (createCss) where

import Data.String.Interpolate (i)
import Relude
import System.Process.Typed (runProcess_)
import UnliftIO (withTempDirectory)

config, input :: ByteString
config = "module.exports = { content: [\"./lib/Bluefin/Dialog/ReflexDom.hs\"] }"
input = "@tailwind base;\n@tailwind components;\n@tailwind utilities;"

createCss :: MonadIO m => m Text
createCss = liftIO do
  withTempDirectory "." "tailwind." \tmpdir -> do
    let inputPath = tmpdir <> "/input.css"; outputPath = tmpdir <> "/output.css"; configPath = tmpdir <> "/config.js"
    writeFileBS inputPath input
    writeFileBS configPath config
    runProcess_ [i|tailwindcss -i #{inputPath} -o #{outputPath} -c #{configPath} -m|]
    decodeUtf8 <$> readFileBS outputPath
