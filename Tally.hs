module Tally where
import Infer
import Dice

-- Generates the P TallySheet distribution for the given dice and cutoff
singleTally :: D -> D -> Int -> Int -> P TallySheet
singleTally d1 d2 cutoff throws =
  let dst1 :: P Int -- sum space
      dst1 = sumDist d1 d2
      dst2 :: P Column -- tally sheet column space
      dst2 = pmap (\sum -> if (sum < cutoff) then LEFT else RIGHT) dst1
      dst3 :: P [Column] -- list of Columns for each trial space
      dst3 = liftPnBag (replicate throws dst2)
      dst4 :: P TallySheet -- TallySheet space
      dst4 = pmap (\xs ->
                          let lft = length (filter ((==) LEFT) xs)
                          in TS lft (throws - lft)
                  ) dst3
  in dst4

-- P(d1 && d2 && TallySheet) -- joint probability of all 3 events
jointTally :: D -> D -> TallySheet -> P D -> Int -> Double
jointTally d1 d2 (TS lthrows rthrows) bag cutoff =
  let throws = lthrows + rthrows
      dst1 :: P (Pair, Int)
      dst1 = draw2bindx bag
      dst2 :: P (Pair, Bool)
      dst2 = pmap (\(pair, sum) -> (pair, sum > cutoff)) dst1
      dst3 :: P ((Pair, Bool), TallySheet)
      dst3 = bindx dst2 (\((Pair dw dx), sumGT) -> singleTally dw dx cutoff throws)
  in probOf ((Pair d1 d2, True), (TS lthrows rthrows)) dst3

