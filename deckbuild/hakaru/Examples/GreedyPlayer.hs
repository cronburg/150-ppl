module Examples.GreedyPlayer where

import Game.DeckBuild.Dominion.Model
import Game.DeckBuild.Dominion.Types
import Examples.First

import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import Control.Monad.State

-- Whether or not player #1 wants to buy a card during this buy phase:
wantToBuy :: Game -> IS.Measure Bool
wantToBuy g = do
    let m = (amtMoney . p1) g
    if m >= 8 then do return True
    else do
        bool <- case m of
            0 -> uncnd $ categorical $ [(True,5),  (False,95)]
            1 -> uncnd $ categorical $ [(True,10), (False,90)]
            2 -> uncnd $ categorical $ [(True,10), (False,90)]
            3 -> uncnd $ categorical $ [(True,65), (False,35)]
            4 -> uncnd $ categorical $ [(True,80), (False,20)]
            5 -> uncnd $ categorical $ [(True,90), (False,10)]
            6 -> uncnd $ categorical $ [(True,97), (False,3)]
            7 -> uncnd $ categorical $ [(True,97), (False,3)]
        return bool

--instance MonadIO Maybe where
--  liftIO = lift . liftIO

greedyBuy :: Game -> IO (Maybe Card)
greedyBuy g = do
  wantACard <- sample1 (wantToBuy g) []
  if wantACard then do
    --c <- liftIO $ sample1 (bestBuy g) []
    c <- sample1 (bestBuy g) []
    return $ Just c
  else
    return $ Nothing

greedyPlayer n = defaultPlayer
  { name = n
  , buyHeuristic = greedyBuy
  }

greedyGame = defaultBaseGame
  { p1 = greedyPlayer "Greedy1"
  , p2 = greedyPlayer "Greedy2"
  }

-- Run our simplistic greedy vs greedy game:
runGreedy :: MonadIO m => m Game
runGreedy = execStateT runGame greedyGame

