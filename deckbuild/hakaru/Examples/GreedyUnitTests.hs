module Examples.GreedyUnitTests where
import Examples.Base
import Examples.Greedy
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Base (baseCardEffects)
import Control.Monad.State

-- TODO: add this module to cabal

firstHandGame = defaultGame
  { p1 = (p1 defaultGame)
    { hand = ((hand.p1) defaultGame)
      { cards = [COPPER,ESTATE,DUCHY,PROVINCE,COPPER] } } }

-- Greedy CELLAR player should discard all three victory cards in her hand:
gTest0 = do
  g <- execStateT (baseCardEffects CELLAR) greedyGame
  return $ all id [elem c ((cards.discardPile.p1) g) | c <- [ESTATE,DUCHY,PROVINCE]]

