module StatusScript.Modules.Klog (warnings) where

import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Time
  ( Day
  , DayOfWeek (..)
  , addDays
  , addGregorianMonthsClip
  , defaultTimeLocale
  , getZonedTime
  , localDay
  , parseTimeM
  , weekFirstDay
  , zonedTimeToLocalTime
  )
import Numeric.Extra (intToDouble)
import Optics hiding ((|>))
import Reflex hiding (mapMaybe)
import Reflex.Host.Headless qualified as R
import Relude
import Shh hiding ((>>>))
import StatusScript.Env
import StatusScript.Mode
import StatusScript.ReflexUtil
import StatusScript.Warnings
import System.Which (which)

newtype KlogLog = MkKlogLog
  { records :: Seq Entry
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data Entry = MkEntry
  { date :: Date
  , diff_mins :: Int
  , total_mins :: Nat
  , should_total :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Date = MkDate {un :: Day} deriving newtype (Eq, Ord, Show)

instance FromJSON Date where
  parseJSON = withText "date" (fmap MkDate . parseTimeM True defaultTimeLocale "%F" . toString)

warnings :: R.MonadHeadlessApp t m => Env -> Dynamic t Mode -> m (Dynamic t [Warning])
warnings env _ = do
  tick <-
    liftIO (which "klog") >>= \case
      Nothing -> pure never
      Just _ -> tickEvent 60
  ev <- performEventThreaded env tick (const getRecords)
  holdDyn Empty ev

getRecords :: IO [Warning]
getRecords = do
  entries <-
    Shh.exe "klog" "json" "-n" |> captureTrim
      <&> (Aeson.decode @KlogLog >>> maybe Empty (.records) >>> toList >>> fmap (\x -> (x.date, x)) >>> Map.fromList)
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  let last_7_days = MkDate . flip addDays today <$> [-6 .. 0]
      missing =
        last_7_days & mapMaybe \day -> case Map.lookup day entries of
          Nothing -> Just $ mkWarning Count [i|Kein Eintrag für #{day}|]
          Just e | not (Text.isSuffixOf "!" e.should_total) -> Just $ mkWarning Count [i|Keine Zielzeit für #{day}|]
          Just e | e.should_total /= "0m!" && e.total_mins == 0 -> Just $ mkWarning Count [i|Keine Zeiteinträge für #{day}|]
          _ -> Nothing
      sums =
        intervals today <&> \(f, t, name) ->
          let
            c = sum $ (.diff_mins) <$> Map.filter (\e -> f <= e.date.un && t >= e.date.un) entries
           in
            mkWarning None [i|#{name}: #{intToDouble ((c * 10) `div` 60) / 10}|]
  pure (missing <> sums)

mkWarning :: BarDisplay -> Text -> Warning
mkWarning warn desc =
  MkWarning
    { description = [desc]
    , heading = "Überstunden"
    , barDisplay = warn
    , group = toEnum 0xeebf -- nf-fa-business_time
    , subgroup = Nothing
    }

intervals :: Day -> [(Day, Day, Text)]
intervals today =
  [ (today, today, "Heute")
  , (pred today, pred today, "Gestern")
  , (weekFirstDay Sunday today, today, "Diese Woche")
  , (addDays (-7) $ weekFirstDay Sunday today, addDays (-7) today, "Letzte Woche")
  , (addGregorianMonthsClip (-1) today, today, "Monat")
  , (addGregorianMonthsClip (-6) today, today, "Halbjahr")
  , (addGregorianMonthsClip (-12) today, today, "Jahr")
  ]
