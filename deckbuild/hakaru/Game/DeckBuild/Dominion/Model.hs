{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Model where
--import Data.List
--import Data.Default.Class
--import Control.Lens

import Game.DeckBuild.Dominion.Types
import Game.Sample
import Control.Monad.State

-- Whether or not the given Card is buy-able in the given supply :: [(Card,Int)]
canBuySupply :: [(Card,Int)] -> Card -> Bool
canBuySupply [] c = False
canBuySupply ((c',cnt'):xs) c = (c' == c && cnt' > 0) || (canBuySupply xs c)

canBuy :: Game -> Card -> Bool
canBuy g c = ((cost c) <= (amtMoney . p1) g) && (canBuySupply ((piles . supply) g) c)

-- Takes all of player #1's discarded cards and shuffles them back into her deck:
--shuffle :: forall m. MonadState Game m => m ()
shuffleCards :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
shuffleCards = do
    g <- get
    newDeck <- liftIO $ shuffleList $ ((cards . discardPile . p1) g) ++ ((cards . deck . p1) g)
    put g { p1 = (p1 g)
            { deck= ((deck.p1) g) {cards=newDeck}
            , discardPile=((discardPile.p1) g) {cards=[]}
            }
          }

-- Player #1 draws n cards from her deck
--draw :: Int -> State (GameState Game) ()
draw :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => Int -> m ()
draw 0 = return ()
draw n = do
    g <- get
    let cs = (cards . deck . p1) g
    case () of
        _ | 0 == length cs -> do
                shuffleCards
                draw n
          | otherwise -> do
                put (g { p1 = (p1 g) { hand = ((hand.p1) g) { cards = (head cs) : ((cards . hand . p1) g) }
                                     , deck = ((deck.p1) g) { cards = tail $ cs } }})
                draw $ n - 1

-- Player #1 discards all remaining cards from her hand and play
discard :: forall (m :: * -> *). MonadState Game m => m ()
discard = do
    g <- get
    let newDiscard = (cards . hand . p1) g ++ (cards . inPlay . p1) g ++ (cards . discardPile . p1) g
    put $ g { p1 = (p1 g)
              { hand        = ((hand.p1) g)        {cards=[]}
              , inPlay      = ((inPlay.p1) g)      {cards=[]}
              , discardPile = ((discardPile.p1) g) {cards=newDiscard}
              }
            }

-- Player #1 and #2 swap places (i.e. p1 == current player)
swapPlayers :: forall (m :: * -> *). MonadState Game m => m ()
swapPlayers = do
    g <- get
    put $ g { p1 = p2 g, p2 = p1 g }

-- TODO: Heuristic for which actions to play? Need more card information to do this...
actionPhase :: forall (m :: * -> *). MonadState Game m => m ()
actionPhase = do
    return ()

findAndDecr c (c',cnt') (c'',cnt'') = if c'' == c then (c'',cnt'' - 1) else (c',cnt')

-- Player #1 buys card c, removing one from the supply and putting into her discard pile
buyCard :: forall (m :: * -> *). MonadState Game m => Card -> m ()
buyCard c = do
    g <- get
    let (c0,cnt0):ss = (piles . supply) g
    let newPilePair = foldl (findAndDecr c) (c0,cnt0 - 1) ss        
    let newSupply   = filter (\(c',_) -> c /= c') $ (piles . supply) g
    put $ g { supply=Supply { piles=newPilePair:newSupply }
            , p1 = (p1 g)
              { discardPile = ((discardPile.p1) g)
                { cards = c : ((cards . discardPile . p1) g)
                }
              }
            }

-- Gets only the treasure cards from a hand:
filterMoney h = filter isTreasure h
filterNotMoney h = filter (not . isTreasure) h

countMoney :: [Card] -> Int
countMoney [] = 0
countMoney (COPPER:xs) = 1 + countMoney xs
countMoney (SILVER:xs) = 2 + countMoney xs
countMoney (GOLD:xs)   = 3 + countMoney xs
countMoney (x:xs)      = countMoney xs

-- Player #1 players all of her money:
playMoney :: forall (m :: * -> *). MonadState Game m => m ()
playMoney = do
    g <- get
    let newInPlay   = (filterMoney    $ (cards . hand . p1) g) ++ (cards . inPlay . p1) g
    let newHand     = filterNotMoney $ (cards . hand . p1) g
    let newAmtMoney = ((amtMoney . p1) g) + (countMoney newInPlay)
    put $ g { p1 = (p1 g)
              { inPlay = ((inPlay.p1) g) { cards=newInPlay }
              , hand   = ((hand.p1) g) { cards=newHand }
              , amtMoney = newAmtMoney
              }
            }

-- Decrements the number of buys player #1 has by n
decrBuys :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => Int -> m ()
decrBuys n = do
  g <- get
  put $ g { p1 = (p1 g) { numBuys = (numBuys . p1) g - n } }

-- TODO: Heuristic for which cards to buy.
buyPhase :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
buyPhase = do
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

-- Executes all phases of player #1's turn:
takeTurn :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
takeTurn = do
    actionPhase
    playMoney
    buyPhase
    discard
    draw 5
    g <- get
    put $ g { p1 = (p1 g) { numActions=1, numBuys=1, amtMoney=0 },
              turn = (turn g) + 1}

-- Whether or not the game is over for the given supply (n == # supply piles found empty already):
endCndn :: Int -> [(Card,Int)] -> Bool
endCndn n ((PROVINCE,0):_) = True          -- Province stack empty - game over
endCndn 0 [] = False                       -- No stacks empty - game not over
endCndn 1 [] = False                       -- One (non-PROVINCE) stack empty - game not over
endCndn 2 [] = False                       -- Two (non-PROVINCE) stacks empty - game not over
endCndn 3 _  = True                        -- Three stacks empty - game over
endCndn n ((c,0):cs) = endCndn (n + 1) cs -- First stack empty - recurse on (n+1)
endCndn n ((c,_):cs) = endCndn n cs       -- First stack NOT empty - recurse on n

-- Game is over if ending condition is true, or turns ran out:
gameOver :: forall (m :: * -> *). MonadState Game m => m Bool
gameOver = do
    g <- get
    return $ (endCndn 0 ((piles.supply) g)) ||
             ((turn g >= 0) && (turn g > maxTurns g))

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

