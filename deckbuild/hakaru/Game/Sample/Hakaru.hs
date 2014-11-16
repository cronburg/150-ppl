{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, BangPatterns #-}
{-# OPTIONS -W #-}
module Game.Sample.Hakaru where

import Language.Hakaru.Types
import Language.Hakaru.Mixture (Prob)
import Language.Hakaru.Mixture (Mixture(..))
import Language.Hakaru.Sampler ()

import qualified System.Random.MWC as MWC
import System.IO.Unsafe
import qualified Data.Map.Strict as M

import Language.Hakaru.ImportanceSampler hiding (sample)

-- Taken from ImportanceSampler.hs in Hakaru (need to modify slightly):
sample :: Measure a -> [Cond] -> IO [(a, Prob)]
sample measure conds = do
  -- TODO: don't recreate this every time we need a sample
  gen <- MWC.createSystemRandom --MWC.create
  unsafeInterleaveIO $ sampleNext gen 
      where once = unMeasure measure conds
            mixToTuple = head . M.toList . unMixture
            sampleNext g = do
              u <- once g
              let x = mixToTuple (finish u)
              xs <- unsafeInterleaveIO $ sampleNext g
              return (x : xs)

