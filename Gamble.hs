module Gamble where
import Infer
import Dice
import Tally

data Payouts = Win Double Double Double Double

-- Computes expected payout for the guesser for one game
expPayout :: P D -> Payouts -> Int -> Int -> Double
expPayout bag (Win g1 g2 g3 g4) throws cutoff =
  let dst0 :: P (Pair, TallySheet)
      dst0 = allTallies bag throws cutoff
      dst1 :: P TallySheet
      dst1 = pmap (\(pr,ts) -> ts) $ dst0
      nthProb :: Int -> P Double -- P Dist over probability of most probable pair
      nthProb n = pmap (\ts -> snd $ mostProb n $ pfilter (\(pr,ts') -> ts == ts') dst0) dst1
      p1 = expected $ nthProb 0
      p2 = expected $ nthProb 1
      p3 = expected $ nthProb 2
      p4 = 1.0 - p1 - p2 - p3
      dst :: P Double
      dst = P[(g1,p1),(g2,p2),(g3,p3),(g4,p4)]
  in expected dst

