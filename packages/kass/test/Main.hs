module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Unsafe
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Kass.Doc
import Kass.Sort
import Numeric.Extra (intToDouble)
import Relude
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify
import Witch (into)

prop :: TestName -> Property' String () -> TestTree
prop = testPropertyWith def{overrideNumTests = Just 1000}

main :: IO ()
main =
  defaultMain $
    -- localOption Verbose $
    testGroup
      "PropertyTests"
      [ prop "docRoundTrip" prop_docRoundTrip
      , prop "reservedFields" prop_reservedFields
      , testGroup
          "Sorting"
          [ prop "sorts" prop_sortSorts
          , prop "is idempotent" prop_sortIdempotent
          , prop "leaves enough space" prop_sortSpacing
          ]
      ]

text :: Gen Text
text =
  into
    <$> list
      (Gen.choose (Gen.inRange (Range.enum (' ', '~'))) (Gen.inRange (Range.enum ('°', 'ÿ'))))

multilineText :: Gen Text
multilineText = Text.unwords <$> list text

list :: Gen a -> Gen [a]
list = Gen.list (Range.between (0, 10))

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.choose (pure Nothing) (Just <$> g)

value :: Gen Value
value =
  Object . KeyMap.fromMapText <$> (into <$> list ((,) <$> text <*> (String <$> text)))

status :: Gen Status
status = Gen.oneof $ (pure Todo) :| [pure Done, pure Checklist, Tag <$> text, Ref . MkId <$> text]

time :: Gen UTCTime
time = posixSecondsToUTCTime . fromInteger . toInteger <$> Gen.int (Range.between (0, 1000000000))

priority :: Gen Double
priority =
  (/ (maxPrio * 10)) . intToDouble
    <$> Gen.int (Range.withOrigin (round (minPrio * maxPrio * 10), round (maxPrio * maxPrio * 10)) 0)

docWORest :: Gen Doc
docWORest = do
  MkDoc
    <$> (MkId <$> text)
    <*> genMaybe text
    <*> Gen.bool False
    <*> multilineText
    <*> genMaybe status
    <*> genMaybe (MkId <$> text)
    <*> (into <$> list text)
    <*> (into <$> list (MkId <$> text))
    <*> genMaybe time
    <*> genMaybe time
    <*> genMaybe ((,) <$> time <*> Gen.int (Range.between (0, 1000)))
    <*> (into <$> list (MkId <$> text))
    <*> priority
    <*> pure mempty

doc :: Gen Doc
doc = do
  d <- docWORest
  rest <- (flip Map.withoutKeys reservedFields . into <$> list ((,) <$> text <*> value))
  pure $ d{rest}

prop_docRoundTrip :: Property ()
prop_docRoundTrip = do
  d <- gen doc
  info $ decodeUtf8 . encode $ d
  let rt = decode @Doc . encode $ d
  assert $ P.satisfies ("isJust", isJust) .$ ("decode . encode", rt)
  assert $
    P.eq
      .$ ("doc", d)
      .$ ("decode . encode $ doc", Unsafe.fromJust rt)

prop_reservedFields :: Property ()
prop_reservedFields = do
  d <- gen doc
  assert $
    P.relatedBy ("isSubsetOf", Set.isSubsetOf)
      .$ ("keysSet . knownFields $ doc", Map.keysSet . KeyMap.toMapText . knownFields $ d)
      .$ ("reservedFields", reservedFields)

prop_sortSorts :: Property ()
prop_sortSorts = do
  d <- Seq.fromList <$> gen (list priority)
  let sorted = fixPriorities d
  assert $
    P.eq
      .$ ("Seq.sort . setPriorities", Seq.sort sorted)
      .$ ("setPriorities", sorted)

prop_sortIdempotent :: Property ()
prop_sortIdempotent = do
  d <- fixPriorities . Seq.fromList <$> gen (list priority)
  assert $
    P.eq
      .$ ("fixPriorities . fixPriorities", fixPriorities d)
      .$ ("fixPriorities", d)

prop_sortSpacing :: Property ()
prop_sortSpacing = do
  d <- Seq.fromList <$> gen (list priority)
  let sorted = fixPriorities d
  info $ "sorted:" <> show sorted
  assert $
    P.satisfies ("all (> minDiff)", all (> minDiff))
      .$ ("sorted_{i+1} - sorted_i", Seq.zipWith (-) (Seq.drop 1 sorted) sorted)
