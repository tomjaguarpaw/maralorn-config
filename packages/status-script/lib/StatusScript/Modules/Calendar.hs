module StatusScript.Modules.Calendar (calendar) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Data.Time qualified as Time
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Relude
import Shh ((|>))
import Shh qualified
import StatusScript.CommandUtil
import StatusScript.CommandUtil qualified as CommandUtil
import StatusScript.Env (Env (..))
import StatusScript.ReflexUtil qualified as ReflexUtil

Shh.load Shh.Absolute ["khal"]

missingExecutables :: IO [FilePath]

data Appointment = MkAppointment
  { start :: Text
  , end :: Text
  , title :: Text
  , description :: Text
  , location :: Text
  , calendar :: Text
  }
  deriving stock (Eq, Generic)
  deriving anyclass (Aeson.ToJSON)

params :: [String]
params =
  [ "list"
  , "-o"
  , "-a"
  , "Standard"
  , "-a"
  , "Planung"
  , "-a"
  , "Uni"
  , "-a"
  , "Maltaire"
  , "now"
  , "2h"
  , "-df"
  , ""
  , "-f"
  , "@=@{start}@@@{end}@@@{title}@@@{description}@@@{location}@@@{calendar}"
  ]

calendar :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Appointment])
calendar = \env -> do
  CommandUtil.reportMissing missingExecutables
  tick <- ReflexUtil.tickEvent (5 * 60)
  pb_ev <- R.getPostBuild
  ReflexUtil.performEventThreaded env (pb_ev <> tick) $ const do
    tdy <- toText . Time.formatTime Time.defaultTimeLocale "%F " <$> getCurrentTime
    appointments <- decodeUtf8 <$> retryIndefinite 10 (khal params |> Shh.captureTrim)
    pure $
      Text.splitOn "@=@" appointments
        & mapMaybe
          ( Text.splitOn "@@@"
              >>> fmap cleanString
              >>> \case
                [start, end, title, description, location, calendar'] ->
                  Just $
                    MkAppointment
                      { start = fromMaybe start $ Text.stripPrefix tdy start
                      , end = fromMaybe start $ Text.stripPrefix tdy end
                      , title
                      , description
                      , location
                      , calendar = calendar'
                      }
                _ -> Nothing
          )

cleanString :: Text -> Text
cleanString = Text.replace "\"" "" . Text.intercalate "\\n" . Text.lines . Text.strip
