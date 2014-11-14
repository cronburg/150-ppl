{-# LANGUAGE DeriveDataTypeable,RankNTypes,FlexibleInstances,
             FlexibleContexts,KindSignatures #-}

module Examples.First where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Model
import Control.Monad.State

-- Recommended initial game setup:
nksupply_init = [(COPPER,60), (SILVER,40), (GOLD,30), (ESTATE,8), (DUCHY,8), (PROVINCE,8)]
kcards_init = [CELLAR,MARKET,MILITIA,MINE,MOAT,REMODEL,SMITHY,VILLAGE,WOODCUTTER,WORKSHOP]
supply_init = (map (\c -> (c,10)) kcards_init) ++ nksupply_init

-- The default game with the default set of cards described in the Dominion rulebook:
defaultBaseGame = defaultGame
  { supply = Pile {
      cards = supply_init
    }
  }

--test0 :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m Game
test0 :: MonadIO m => m Game
test0 = execStateT (do {shuffleDrawSwap; shuffleDrawSwap }) defaultBaseGame

test1 :: MonadIO m => m Bool
test1 = evalStateT gameOver defaultBaseGame

test2 :: MonadIO m => m Game
test2 = execStateT runGame defaultBaseGame

