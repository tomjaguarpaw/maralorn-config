module StatusScript.Modules.Bluetooth where

import Maralorn.Prelude

import Data.Text qualified as Text
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh ((|>))
import Shh qualified
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["bluetoothctl"]
missingExecutables :: IO [FilePath]
bluetooth :: R.MonadHeadlessApp t m => R.Event t () -> m (R.Event t [Text])
bluetooth event = do
  ReflexUtil.performEventThreaded event $ const do
    bluetoothctl "devices" "Connected"
      |> Shh.captureTrim
      <&> decodeUtf8
      % lines
      %> Text.drop 25 -- "Device E8:EE:CC:XX:XX:XX Soundcore Life Q30"
