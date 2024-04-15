module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Unsafe
import Data.Set qualified as Set
import Kass.Doc
import Maralude
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify

main :: IO ()
main =
  defaultMain
    $ localOption Verbose
    $ testGroup
      "PropertyTests"
      [ testProperty "docRoundTrip" prop_docRoundTrip
      , testProperty "reservedFields" prop_reservedFields
      ]

text :: Gen Text
text =
  into
    <$> list
      (Gen.choose (Gen.inRange (Range.enum (' ', '~'))) (Gen.inRange (Range.enum ('°', 'ÿ'))))

multilineText :: Gen Text
multilineText = list text ^. mapping (re worded)

list :: Gen a -> Gen [a]
list = Gen.list (Range.between (0, 5))

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.choose (pure Nothing) (Just <$> g)

value :: Gen Value
value =
  Object . KeyMap.fromMapText <$> (into <$> list ((,) <$> text <*> (String <$> text)))

docWORest :: Gen Doc
docWORest = do
  MkDoc
    <$> (MkId <$> text)
    <*> genMaybe text
    <*> Gen.bool False
    <*> multilineText
    <*> genMaybe (MkId <$> text)
    <*> (into <$> list text)
    <*> pure mempty

doc :: Gen Doc
doc = do
  d <- docWORest
  rest <- (flip Map.withoutKeys reservedFields . into <$> list ((,) <$> text <*> value))
  pure $ d{rest}

prop_docRoundTrip :: Property ()
prop_docRoundTrip = do
  d <- gen doc
  let rt = decode @Doc . encode $ d
  assert $ P.satisfies ("isJust", isJust) .$ ("decode . encode", rt)
  assert
    $ P.eq
    .$ ("doc", d)
    .$ ("decode . encode $ doc", Unsafe.fromJust rt)

prop_reservedFields :: Property ()
prop_reservedFields = do
  d <- gen doc
  assert
    $ P.relatedBy ("isSubsetOf", Set.isSubsetOf)
    .$ ("keysSet . knownFields $ doc", Map.keysSet . KeyMap.toMapText . knownFields $ d)
    .$ ("reservedFields", reservedFields)
