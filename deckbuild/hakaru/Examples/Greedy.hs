module Examples.Greedy where
-- TODO: Move this into an "AI" module under "Dominion"

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample.Sample
import Haskell.Macros
import Examples.Base


import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

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
  GOLD -> 100; SILVER -> 80; VILLAGE -> 60; MOAT -> 0; CELLAR -> 1;
  PROVINCE -> 1000; otherwise -> 0

wantCard :: Game -> Card -> Bool
wantCard g c = case c of
  SILVER    -> True
  GOLD      -> True
  DUCHY     -> True
  PROVINCE  -> True
  VILLAGE   -> True
  SMITHY    -> True
  CELLAR    -> True
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

greedyAct :: Game -> IO (Maybe Card)
greedyAct g = do
  let as = filter isAction $ (cards.hand.p1) g
  case length as of
    0 -> return Nothing
    _ -> if elem VILLAGE as
         then return $ Just VILLAGE
         else return $ Just $ maximumBy (comparing cost) as

cellarPick g = maybeHead $ filter isVictory ((cards.hand.p1) g)

-- c' == the card which caused us to have to maybe pick a card
greedyMayPick :: Game -> Card -> IO (Maybe Card)
greedyMayPick g c' = return $ case c' of
  CELLAR     -> cellarPick g -- pick a victory card in hand if exists
  CHANCELLOR -> Just COPPER  -- any card triggers a discard deck
  otherwise  -> Nothing

-- c' == the card which caused us to have to pick a card
greedyMustPick :: Game -> Card -> IO Card
greedyMustPick g c' = undefined

greedyPlayer n = defaultPlayer
  { name = n
  , buyHeuristic = greedyBuy
  , actHeuristic = greedyAct
  , mayPick      = greedyMayPick
  , mustPick     = greedyMustPick
  }

greedyGame = defaultBaseGame
  { p1 = greedyPlayer "Greedy1"
  , p2 = greedyPlayer "Greedy2"
  }

-- Run our simplistic greedy vs greedy game:
runGreedy :: MonadIO m => m Game
runGreedy = execStateT runGame greedyGame

