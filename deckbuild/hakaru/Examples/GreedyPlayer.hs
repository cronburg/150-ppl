module Examples.GreedyPlayer where

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample
import Examples.Base

import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import Control.Monad.State

-- Whether or not player #1 wants to buy a card during this buy phase:
wantToBuy :: Game -> IS.Measure Bool
wantToBuy g = do
    let m = (amtMoney . p1) g
    if      m <= 2 then do return False
    else if m >= 6 then do return True
    else do
        bool <- case m of
            3 -> uncnd $ categorical $ [(True,65), (False,35)]
            4 -> uncnd $ categorical $ [(True,80), (False,20)]
            5 -> uncnd $ categorical $ [(True,90), (False,10)]
        return bool

--instance MonadIO Maybe where
--  liftIO = lift . liftIO

-- Perceived value of a card c if player #1 in game g were to buy it
cardValue :: Game -> Card -> Double
--cardValue g c = fI $ cost c
cardValue g c = case c of
  GOLD -> 100; SILVER -> 80; MOAT -> 0; CELLAR -> 0;
  PROVINCE -> 1000; otherwise -> 0

wantCard :: Game -> Card -> Bool
wantCard g c = case c of
  SILVER    -> True
  GOLD      -> True
  DUCHY     -> True
  PROVINCE  -> True
  otherwise -> False

-- What card should be bought in game state g by player #1
bestBuy :: Game -> IS.Measure Card
bestBuy g = do
    card <- uncnd $ categorical $
      [(c, cardValue g c) |                   -- Categorical value
        c <- ((map fst) . piles . supply) g,  -- Cards in supply
        wantCard g c && canBuy g c]           -- Buy conditions
    return card

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

