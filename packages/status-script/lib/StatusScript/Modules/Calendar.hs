module StatusScript.Modules.Calendar (calendar) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Maralorn.Prelude
import Reflex qualified as R
import Reflex.Host.Headless qualified as R
import Shh qualified
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

calendar :: R.MonadHeadlessApp t m => Env -> m (R.Event t [Appointment])
calendar = \env -> do
  CommandUtil.reportMissing missingExecutables
  tick <- ReflexUtil.tickEvent (5 * 60)
  start <- R.getPostBuild
  ReflexUtil.performEventThreaded env (start <> tick) $ const do
    appointments <-
      decodeUtf8
        <$> CommandUtil.tryCmd
          ( khal
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
          )
    pure $
      appointments
        & Text.splitOn "@=@"
          %> Text.splitOn "@@@"
          %>> cleanString
          % mapMaybe \case
            [start', end, title, description, location, calendar'] ->
              Just $
                MkAppointment
                  { start = start'
                  , end
                  , title
                  , description
                  , location
                  , calendar = calendar'
                  }
            _ -> Nothing

cleanString :: Text -> Text
cleanString = Text.replace "\"" "" . Text.intercalate "\\n" . Text.lines . Text.strip
