module Bluefin.Dialog.ReflexDom.TH (createCss) where

import Data.String.Interpolate (i, __i)
import Relude
import System.Process.Typed (runProcess_)
import UnliftIO (withTempDirectory)

config, input :: ByteString
config =
  [__i|
  module.exports = {
    content: ["./lib/Bluefin/Dialog/ReflexDom.hs"],
    theme: {
      fontFamily: {
        'serif': ["Libertinus","Symbols Nerd Font"],
        'sans': ["B612","Symbols Nerd Font"]
      }
    }
  }
|]
input =
  [__i|
  @tailwind base;
  @tailwind components;
  @tailwind utilities;

  @layer base {
    @font-face {
      font-family: 'Symbols Nerd Font';
      font-style: normal;
      font-weight: 400;
      font-display: swap;
      src: url(/fonts/truetype/NerdFonts/SymbolsNerdFont-Regular.ttf);
    }
  }
|]

createCss :: MonadIO m => m Text
createCss = liftIO do
  withTempDirectory "." "tailwind." \tmpdir -> do
    let inputPath = tmpdir <> "/input.css"; outputPath = tmpdir <> "/output.css"; configPath = tmpdir <> "/config.js"
    writeFileBS inputPath input
    writeFileBS configPath config
    runProcess_ [i|tailwindcss -i #{inputPath} -o #{outputPath} -c #{configPath} -m|]
    decodeUtf8 <$> readFileBS outputPath
