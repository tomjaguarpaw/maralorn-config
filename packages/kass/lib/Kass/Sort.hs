module Kass.Sort (setPriorities, fixPriorities, maxPrio, minPrio, minDiff) where

import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Kass.Doc
import Optics
import Relude

maxPrio, minPrio, minDiff :: Double
maxPrio = 1_000_000
minPrio = -maxPrio
minDiff = 1 / maxPrio

setPriorities :: Seq Doc -> Docs
setPriorities xs =
  Map.fromList
    . catMaybes
    . toList
    . Seq.zipWith
      ( \cases
          d new
            | d.priority /= new -> Just (d.id, d & #priority .~ new)
            | otherwise -> Nothing
      )
      xs
    . fixPriorities
    . fmap (.priority)
    $ xs

fixPriorities :: Seq Double -> Seq Double
fixPriorities = setPrios . unsetPrios . fmap (Just)

setPrios :: Seq (Maybe Double) -> Seq Double
setPrios =
  fmap (either (\(lprio, ldist, rprio, rdist) -> (lprio * rdist + rprio * ldist) / (ldist + rdist)) id)
    . (\xs -> Seq.deleteAt (length xs - 1) xs)
    . Seq.scanr
      ( \cases
          (Left (lprio, ldist)) (Left (_, _, rprio, rdist)) -> Left (lprio, ldist, rprio, rdist + 1)
          (Left (lprio, ldist)) (Right rprio) -> Left (lprio, ldist, rprio, 1)
          (Right prio) _ -> Right prio
      )
      (Right maxPrio)
    . Seq.drop 1
    . Seq.scanl
      ( \cases
          (Left (prio, dist)) Nothing -> Left (prio, dist + 1)
          (Right prio) Nothing -> Left (prio, 1)
          _ (Just prio) -> Right prio
      )
      (Right minPrio)

unsetPrios :: Seq (Maybe Double) -> Seq (Maybe Double)
unsetPrios xs = case findWorst xs of
  Just i -> unsetPrios (xs & ix i .~ Nothing)
  Nothing -> xs

data PrioSearch = MkPrioSearch
  { worst :: Maybe (Int, Double)
  , before :: (Int, Double)
  , scrutinee :: Maybe (Int, Double)
  }
  deriving stock (Eq, Show, Generic)

findWorst :: Seq (Maybe Double) -> Maybe Int
findWorst xs =
  preview (#worst % _Just % _1) $
    accumulate
      (Seq.foldlWithIndex accumulate (MkPrioSearch Nothing (-1, minPrio) Nothing) xs)
      (Seq.length xs)
      (Just maxPrio)

accumulate :: PrioSearch -> Int -> Maybe Double -> PrioSearch
accumulate ps ind = \case
  Nothing -> ps
  Just prio -> set #scrutinee (Just (ind, prio)) $ case ps.scrutinee of
    Nothing -> ps
    Just scr ->
      set #before scr $
        case badness ps.before scr (ind, prio) of
          Just bad | bad > maybe (-1) snd ps.worst -> ps & #worst ?~ (fst scr, bad)
          _ -> ps

badness :: (Int, Double) -> (Int, Double) -> (Int, Double) -> Maybe Double
badness bef@(_, befPrio) scr@(_, scrPrio) aft@(_, aftPrio) =
  if (is_bad bef scr || is_bad scr aft)
    then Just $ abs (2 * scrPrio - befPrio - aftPrio)
    else Nothing
 where
  is_bad :: (Int, Double) -> (Int, Double) -> Bool
  is_bad (leftInd, leftPrio) (rightInd, rightPrio) =
    leftPrio + minDiff * fromIntegral (rightInd - leftInd) >= rightPrio
