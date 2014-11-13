module Game.DeckBuild.Dominion.Model where
import Game.DeckBuild.Dominion.Types

-- Importance Sampler macros:
sample1 :: (Show a, Ord a) => IS.Measure a -> [Cond] -> IO a
sample1 fncn conds = do
    s <- sampleN 1 fncn conds
    return $ head s

sampleN :: (Show a, Ord a) => Int -> IS.Measure a -> [Cond] -> IO [a]
sampleN n fncn conds = do
    t <- IS.sample fncn conds
    return $ take n $ map fst t

uncnd = IS.unconditioned
cnd  = IS.conditioned

-- Whether or not the given Card is buy-able in the given supply :: [(Card,Int)]
canBuySupply :: [(Card,Int)] -> Card -> Bool
canBuySupply [] c = False
canBuySupply ((c',cnt'):xs) c = (c' == c && cnt' > 0) || (canBuySupply xs c)

canBuy :: Game -> Card -> Bool
canBuy g c = ((cost c) <= (amtMoney . p1) g) && (canBuySupply (supply g) c)

-- Perceived value of a card c if player #1 in game g were to buy it
cardValue :: Game -> Card -> Double
cardValue g c = fI $ cost c

-- What card should be bought in game state g by player #1
bestBuy :: Game -> IS.Measure Card
bestBuy g = do
    card <- uncnd $ categorical $ [(c, cardValue g c) | c <- ((map fst) . supply) g, canBuy g c]
    return card

-- Get a uniform int on a mn-closed mx-open interval [mn,mx)
uniformInt :: Int -> Int -> IS.Measure Int
uniformInt mn' mx' = do
    let (mn,mx) = (fI mn', fI mx')
    dbl <- uncnd $ uniform mn mx
    if (dbl == mx) then return $ truncate mx
    else return $ floor dbl

-- Shuffling using hakaru:
--shuffleC' [] d2 = return d2
--shuffleC' d1 d2 = do
--    idx <- uniformInt 0 (length d1)
--    return $ (d1 !! idx) : (removeNth idx d1)
--shuffleCards' d = sample1 (shuffleC' d []) []

-- Shuffling using System.Random:
shuffleCards' d = do
    g <- newStdGen
    return $ shuffle' d (length d) g

-- Takes all of player #1's discarded cards and shuffles them back into her deck:
--shuffle :: forall m. MonadState Game m => m ()
shuffleCards = do
    g <- get
    newDeck <- liftIO $ shuffleCards' $ ((discardPile . p1) g) ++ ((deck . p1) g)
    put g { p1 = ((p1 g) { deck=newDeck, discardPile=[] }) }

-- Player #1 draws n cards from her deck
--draw :: Int -> State (GameState Game) ()
draw 0 = return 0
draw n = do
    g <- get
    let cs = (deck . p1) g
    case () of
        _ | 0 == length cs -> do
                shuffleCards
                draw n
          | otherwise -> do
                put (g { p1 = (p1 g) { hand = (head cs) : ((hand . p1) g),
                                       deck = tail $ cs } })
                draw $ n - 1

-- Player #1 discards all remaining cards from her hand and play
discard = do
    g <- get
    let newDiscard = (hand . p1) g ++ (inPlay . p1) g ++ (discardPile . p1) g
    put $ g { p1 = (p1 g) { hand=[], inPlay=[], discardPile=newDiscard} }

-- Player #1 and #2 swap places (i.e. p1 == current player)
swapPlayers = do
    g <- get
    put $ g { p1 = p2 g, p2 = p1 g }

-- TODO: Heuristic for which actions to play? Need more card information to do this...
actionPhase = do
    return ()

findAndDecr c (c',cnt') (c'',cnt'') = if c'' == c then (c'',cnt'' - 1) else (c',cnt')

-- Player #1 buys card c, removing one from the supply and putting into her discard pile
buyCard c = do
    g <- get
    let (c0,cnt0):ss = supply g
    let newPilePair = foldl (findAndDecr c) (c0,cnt0 - 1) ss        
    let newSupply   = filter (\(c',_) -> c /= c') (supply g)
    put $ g { supply=newPilePair:newSupply, p1 = (p1 g) { discardPile = c : ((discardPile . p1) g)} }

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
playMoney = do
    g <- get
    let newInPlay   = (filterMoney    $ (hand . p1) g) ++ (inPlay . p1) g
    let newHand     = filterNotMoney $ (hand . p1) g
    let newAmtMoney = ((amtMoney . p1) g) + (countMoney newInPlay)
    put $ g { p1 = (p1 g) { inPlay = newInPlay, hand = newHand, amtMoney = newAmtMoney } }



-- Whether or not player #1 wants to buy a card during this buy phase:
wantToBuy g = do
    let m = (amtMoney . p1) g
    if m >= 8 then do return True
    else do
        bool <- case m of
            0 -> uncnd $ categorical $ [(True,5),  (False,95)]
            1 -> uncnd $ categorical $ [(True,10), (False,90)]
            2 -> uncnd $ categorical $ [(True,50), (False,50)]
            3 -> uncnd $ categorical $ [(True,65), (False,35)]
            4 -> uncnd $ categorical $ [(True,80), (False,20)]
            5 -> uncnd $ categorical $ [(True,90), (False,10)]
            6 -> uncnd $ categorical $ [(True,97), (False,3)]
            7 -> uncnd $ categorical $ [(True,97), (False,3)]
        return bool

-- TODO: Heuristic for which cards to buy.
buyPhase = do
    playMoney
    g <- get
    wantACard <- liftIO $ sample1 (wantToBuy g) []
    if wantACard then do
        g <- get
        c <- liftIO $ sample1 (bestBuy g) []
        buyCard c
    else do
        return ()

-- Executes all phases of player #1's turn:
takeTurn = do
    actionPhase
    buyPhase
    discard
    draw 5
    g <- get
    put $ g { p1 = (p1 g) { numActions=1, numBuys=1, amtMoney=0 },
              turn = (turn g) + 1}

-- Whether or not the game is over for the given supply (n == # supply piles found empty already):
_endCndn :: Int -> [(Card,Int)] -> Bool
_endCndn 0 [] = False                       -- No stacks empty - game not over
_endCndn 1 [] = False                       -- One (non-PROVINCE) stack empty - game not over
_endCndn n ((PROVINCE,0):_) = True          -- Province stack empty - game over
_endCndn 2 _ = True                         -- Found two stacks empty - game over
_endCndn n ((c,0):cs) = _endCndn (n + 1) cs -- First stack empty - recurse on (n+1)
_endCndn n ((c,_):cs) = _endCndn n cs       -- First stack NOT empty - recurse on n

gameOver = do
    g <- get
    return $ _endCndn 0 (supply g)

shuffleDrawSwap = do
    shuffleCards
    draw 5
    swapPlayers

runGameLoop = do
    takeTurn
    swapPlayers
    g <- get
    if ((turn g) == 100) then return ()
    else runGameLoop

-- Run the game:
runGame = do
    shuffleDrawSwap
    shuffleDrawSwap
    runGameLoop


