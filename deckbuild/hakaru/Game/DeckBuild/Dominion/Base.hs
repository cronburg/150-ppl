{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Base where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Engine
import Control.Monad.State


addMoney :: forall (m :: * -> *). MonadState Game m => Int -> m ()
addMoney n = get >>= (\g -> put $ g { p1 = (p1 g) { amtMoney = ((amtMoney . p1) g) + n } })

addActions :: forall (m :: * -> *). MonadState Game m => Int -> m ()
addActions n = get >>= (\g -> put $ g { p1 = (p1 g) { numActions = ((numActions . p1) g) + n } })

addBuys :: forall (m :: * -> *). MonadState Game m => Int -> m ()
addBuys n = get >>= (\g -> put $ g { p1 = (p1 g) { numBuys = ((numBuys.p1) g) + n } })

nop :: forall (m :: * -> *). MonadState Game m => m ()
nop = return ()

gain :: forall (m :: * -> *). MonadState Game m => Card -> m ()
gain c = buyCard c -- TODO - gain source and destination

trashCard :: forall (m :: * -> *). MonadState Game m => Card -> m ()
trashCard c = nop -- TODO - source and destination

playCard :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => Card -> m ()
playCard c = case c of
  COPPER     -> addMoney 1
  SILVER     -> addMoney 2
  GOLD       -> addMoney 3
  ESTATE     -> nop
  DUCHY      -> nop
  PROVINCE   -> nop
  CELLAR     -> nop -- TODO: may discard
  CHAPEL     -> nop -- TODO: may trash
  MOAT       -> draw 2
  CHANCELLOR -> addMoney 2 -- TODO: may discard
  VILLAGE    -> draw 1 >> addActions 2
  WOODCUTTER -> addBuys 1 >> addMoney 2
  WORKSHOP   -> nop -- TODO: ask gain card costing 4
  BUREAUCRAT -> gain SILVER -- TODO: rest of action
  FEAST      -> trashCard c -- TODO: gain card costing up to $5
  GARDENS    -> nop
  MILITIA    -> addMoney 2 -- TODO: other player chooses cards to discard
  MONEYLENDER -> nop -- TODO
  REMODEL    -> nop -- TODO
  SMITHY     -> draw 3
  SPY        -> draw 1 >> addActions 1 -- TODO
  THIEF      -> nop -- TODO
  THRONEROOM -> nop -- TODO
  COUNCILROOM -> draw 4 >> addBuys 1 >> swapPlayers >> draw 4 >> swapPlayers
  FESTIVAL   -> addActions 2 >> addBuys 1 >> addMoney 2
  LABORATORY -> draw 2 >> addActions 1
  LIBRARY    -> nop -- TODO
  MARKET     -> draw 1 >> addActions 1 >> addBuys 1 >> addMoney 1
  MINE       -> nop -- TODO
  WITCH      -> draw 2 >> swapPlayers >> gain CURSE >> swapPlayers
  ADVENTURER -> nop -- TODO

