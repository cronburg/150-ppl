{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Base (baseCardEffects) where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Lib
import Control.Monad.State

-- Discards any number of cards, returning the number of cards discarded
cellarEffect' :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m Int
cellarEffect' = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g CELLAR
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then discard c >> cellarEffect' >>= \n -> return $ n + 1
               else return 0
    Nothing -> return 0

-- Discard any number of cards, then draw that many cards:
cellarEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
cellarEffect = addActions 1 >> cellarEffect' >>= \n -> draw n

-- Trash up to 4 cards
-- n == # of cards trashed so far
chapelEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => Int -> m Int
chapelEffect 4 = return 0
chapelEffect n = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g CHAPEL
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then trashCard c >> chapelEffect (n + 1) >>= \n' -> return $ n' + 1
               else return 0
    Nothing -> return 0

baseCardEffects :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => Card -> m ()
baseCardEffects c = do
 case c of
  COPPER     -> addMoney 1
  SILVER     -> addMoney 2
  GOLD       -> addMoney 3
  ESTATE     -> nop
  DUCHY      -> nop
  PROVINCE   -> nop
  CELLAR     -> cellarEffect
  CHAPEL     -> chapelEffect 0 >> return ()
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

