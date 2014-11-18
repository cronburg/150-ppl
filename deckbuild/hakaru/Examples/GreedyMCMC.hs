module Examples.GreedyMCMC where

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample.Sample
import Examples.Base
import Examples.GreedySampler

import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

import System.IO.Unsafe (unsafePerformIO)

greedyModel :: Measure Int
greedyModel = do
  param0 <- unconditioned $ uniform 0 1
  let param1 = 1 - param0
  let g = unsafePerformIO $ runGreedy (param0,param1)
  return $ turn g

main n = do
  --samples <- IS.empiricalMeasure 10 greedyModel [
  samples <- mcmc greedyModel []
  return $ take n samples

