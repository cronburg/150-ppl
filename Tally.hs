module Tally where
import Infer
import Dice

-- Determines which column a tick goes in
getCol :: Int -> Int -> Column
getCol cutoff sum
  | sum < cutoff = LEFT
  | otherwise    = RIGHT

-- Like an if statement, but with Columns instead of Bools
chooseCol :: Column -> a -> a -> a
chooseCol col lft rt
  | col == LEFT = lft
  | otherwise   = rt

-- Generates the P TallySheet distribution for the given dice and cutoff
singleTally :: D -> D -> Int -> Int -> P TallySheet
singleTally d1 d2 throws cutoff =
  let dst1 :: P Int -- sum space
      dst1 = sumDist d1 d2
      dst2 :: P Column -- tally sheet column space
      dst2 = pmap (getCol cutoff) dst1
      dst3 :: P [Column] -- list of Columns for each trial space
      dst3 = liftPnBag (replicate throws dst2)
      dst4 :: P TallySheet -- TallySheet space
      dst4 = pmap (\xs ->
                          let lft = length (filter ((==) LEFT) xs)
                          in TS lft (throws - lft)
                  ) dst3
  in dst4

allTallies :: P D -> Int -> Int -> P (Pair, TallySheet)
allTallies bag throws cutoff =
  let dst1 :: P Pair -- lift and map bag to pair space
      dst1 = allPairs bag
      dst2 :: P (Pair, TallySheet) -- (pair, tallies) space
      dst2 = bindx dst1 (\(Pair dw dx) -> singleTally dw dx throws cutoff)
  in dst2

-- P(d1 && d2 && TallySheet) -- joint probability of all 3 events
jointTally :: D -> D -> TallySheet -> P D -> Int -> Double
jointTally d1 d2 (TS lthrows rthrows) bag cutoff =
  let throws = lthrows + rthrows
  in probOf (Pair d1 d2, (TS lthrows rthrows)) (allTallies bag throws cutoff)

expectedMarks :: P D -> Column -> Int -> Int -> Double
expectedMarks bag col throws cutoff =
  let dst1 :: P (Pair, TallySheet) -- (pair,tallies) space
      dst1 = allTallies bag throws cutoff
      dst2 :: P Int
      dst2 = pmap (\(pr,TS lft rt) -> chooseCol col lft rt) dst1
  in expected dst2

