{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Engine where
--import Data.List
--import Data.Default.Class
--import Control.Lens

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Types

import Game.Sample.Sample
import Control.Monad.State

doPhase numThing thingHeuristic addThing canDo doCard isThing = do
  g <- get
  if (numThing . p1) g == 0 then return () else do
    c' <- liftIO $ ((thingHeuristic.p1) g) g
    case c' of
      Just c  -> do
        if (isThing c) && (canDo g c) then addThing (-1) >> doCard c else return ()
        doPhase numThing thingHeuristic addThing canDo doCard isThing
      Nothing -> return ()

-- TODO: Run actHeuristic of player #1:
actionPhase :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
actionPhase = doPhase numActions actHeuristic addActions canPlay playCard isAction
{-
  g <- get
  if (numActions . p1) g == 0 then return () else do
    c' <- liftIO $ ((actHeuristic . p1) g) g
    addActions (-1)
    g' <- get
    case c' of
      Just c  -> if canPlay g c then playCard c >> actionPhase else return ()
      Nothing -> return ()
    return ()
-}

buyPhase :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
buyPhase = doPhase numBuys buyHeuristic addBuys canBuy buyCard isSupply
{-
  g <- get
  -- If no more buys, return, else ask player what card they want
  if (numBuys . p1) g == 0 then return () else do
    c' <- liftIO $ ((buyHeuristic . p1) g) g
    decrBuys 1
    -- Not really necessary, but doing it for safety since decrBuys affects the
    -- state, and I might e.g. change how canBuy works in the future:
    g' <- get
    case c' of
      -- If you can buy the card, buy it, else ignore the ignorant player trying to steal cards:
      Just c  -> if canBuy g c then buyCard c >> buyPhase else return ()
      -- Player said "I don't want anything" - end the buy phase:
      Nothing -> return ()
-}

moneyPhase :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
moneyPhase = doPhase (countMoney . cards . hand) moneyHeuristic (const $ return ())
                      canPlay playCard isTreasure

-- Executes all phases of player #1's turn:
takeTurn :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
takeTurn = do
    actionPhase
    moneyPhase
    buyPhase
    discardAll
    draw 5
    g <- get
    put $ g { p1 = (p1 g) { numActions=1, numBuys=1, amtMoney=0 },
              turn = (turn g) + 1}

shuffleDrawSwap :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
shuffleDrawSwap = do
    shuffleCards
    draw 5
    swapPlayers

runGameLoop :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
runGameLoop = do
    takeTurn
    swapPlayers
    g <- get
    --if ((turn g) == 100) then return ()
    over <- gameOver
    if over then do return ()
    else do runGameLoop

-- Run the game:
runGame :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
runGame = do
    shuffleDrawSwap
    shuffleDrawSwap
    runGameLoop

