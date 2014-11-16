{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Types where
import Data.Typeable
import Control.Monad.State
import Data.List
import Data.Ord (comparing)

-- Kingdom Card class type
-- (Eq a, Ord a, Typeable a, Show a, Enum a) => 

data Card =
  -- Non-Kingdom cards:
    COPPER | SILVER | GOLD | CURSE | ESTATE | DUCHY | PROVINCE
  -- Base cards:
  | CELLAR      | CHAPEL     | MOAT    | CHANCELLOR | VILLAGE    | WOODCUTTER
  | WORKSHOP    | BUREAUCRAT | FEAST   | GARDENS    | MILITIA    | MONEYLENDER
  | REMODEL     | SMITHY     | SPY     | THIEF      | THRONEROOM | COUNCILROOM
  | FESTIVAL    | LABORATORY | LIBRARY | MARKET     | MINE       | WITCH
  | ADVENTURER
  -- Intrigue cards:
  | COURTYARD   | PAWN      | SECRETCHAMBER | GREATHALL | MASQUERADE | SHANTYTOWN
  | STEWARD     | SWINDLER  | WISHINGWELL   | BARON     | BRIDGE     | CONSPIRATOR
  | COPPERSMITH | IRONWORKS | MININGVILLAGE | SCOUT     | DUKE       | MINION
  | SABOTEUR    | TORTURER  | TRADINGPOST   | TRIBUTE   | UPGRADE    | HAREM
  | NOBLES deriving (Eq, Ord, Typeable, Show, Enum, Bounded)

-- Alternative implentation using:
-- cost c = (!!) [0, 3, 6, 2, ...] (fromEnum c)
cost c = case c of
  -- Non-Kingdom card costs:
  COPPER -> 0;      SILVER -> 3;      GOLD -> 6;          ESTATE -> 2;        DUCHY -> 5;      PROVINCE -> 8
  -- Base card costs:
  CELLAR -> 2;      CHAPEL -> 2;      MOAT -> 2;          CHANCELLOR -> 3;    VILLAGE -> 3;    WOODCUTTER -> 3
  WORKSHOP -> 3;    BUREAUCRAT -> 4;  FEAST -> 4;         GARDENS -> 4;       MILITIA -> 4;    MONEYLENDER -> 4
  REMODEL -> 4;     SMITHY -> 4;      SPY -> 4;           THIEF -> 4;         THRONEROOM -> 4; COUNCILROOM -> 5
  FESTIVAL -> 5;    LABORATORY -> 5;  LIBRARY -> 5;       MARKET -> 5;         MINE -> 5;       WITCH -> 5;
  ADVENTURER -> 6
  -- Intrigue card costs:
  COURTYARD -> 2;   PAWN -> 2;        SECRETCHAMBER -> 2; GREATHALL -> 3; MASQUERADE -> 3; SHANTYTOWN -> 3
  STEWARD -> 3;     SWINDLER -> 3;    WISHINGWELL -> 3;   BARON -> 4;     BRIDGE -> 4;     CONSPIRATOR -> 4;
  COPPERSMITH -> 4; IRONWORKS -> 4;   MININGVILLAGE -> 4; SCOUT -> 4;     DUKE -> 5;       MINION -> 5;
  SABOTEUR -> 5;    TORTURER -> 5;    TRADINGPOST -> 5;   TRIBUTE -> 5
  UPGRADE -> 5;     HAREM -> 6;       NOBLES -> 6

-- ALL possible cards
allCards :: [Card]
allCards = [minBound..]

bCards  = -- Base Cards:
  [CELLAR, CHAPEL, MOAT, CHANCELLOR, VILLAGE, WOODCUTTER
  , WORKSHOP, BUREAUCRAT, FEAST, GARDENS, MILITIA, MONEYLENDER
  , REMODEL, SMITHY, SPY, THIEF, THRONEROOM, COUNCILROOM
  , FESTIVAL, LABORATORY, LIBRARY, MARKET, MINE, WITCH
  , ADVENTURER]

iCards = -- Intrigue Cards:
  [COURTYARD, PAWN, SECRETCHAMBER, GREATHALL, MASQUERADE, SHANTYTOWN
  , STEWARD, SWINDLER, WISHINGWELL, BARON, BRIDGE, CONSPIRATOR
  , COPPERSMITH, IRONWORKS, MININGVILLAGE, SCOUT, DUKE, MINION
  , SABOTEUR, TORTURER, TRADINGPOST, TRIBUTE, UPGRADE, HAREM
  , NOBLES]

actionCards = bCards ++ iCards
isAction :: Card -> Bool
isAction c = elem c actionCards

kingdomCards = bCards ++ iCards
isKingdom :: Card -> Bool
isKingdom c = elem c kingdomCards

treasureCards = [COPPER,SILVER,GOLD]
isTreasure :: Card -> Bool
isTreasure c = elem c treasureCards

victoryCards = [ESTATE,DUCHY,PROVINCE]
isVictory :: Card -> Bool
isVictory c = elem c victoryCards

-- Non-Kingdom Cards:
nkCards = treasureCards ++ victoryCards

supplyCards = nub $ kingdomCards ++ nkCards
isSupply :: Card -> Bool
isSupply c = elem c supplyCards

-- TODO: setup / write SYB or TemplateHaskell to auto-create the above data type definitions

-- Data & type definitions:
data Pile  = Pile
  { cards      :: [Card]   -- The list of cards in this pile
  , visibleTo  :: [Player] -- List of players this pile is visible to
  -- Function for sorting this pile (e.g. for printing):
  --, sortPileBy :: Ord a => Maybe (Card -> a)
  , sortPileBy :: Maybe (Card -> Int)
  }

instance Show Pile where
  -- If sortPileBy is Just sPB, sort with sPB:
  show (Pile { cards = cs, sortPileBy = Just sPB }) = show $ sortBy (comparing $ sPB) cs
  -- Do not sort if the sortPileBy is Nothing:
  show (Pile { cards = cs }) = show cs

instance Eq Pile where
  p == p' = all id [cards p == cards p', visibleTo p == visibleTo p']

{-
  -- Show cars in-a-pile in the order they are in the pile:
  show (Pile p)    = show $ p
  -- Show cards in-hand in categorically sorted order:
  show (Hand h)    = show $ sortBy (\a b -> (fromEnum a) < (fromEnum b)) h
  -- Show cards in-play in the order they were played:
  show (PlayPile pp) = show $ pp
-}

data Supply = Supply
  { piles :: [(Card,Int)]
  } deriving (Eq)
instance Show Supply where
  -- Show supply piles in cost-sorted order:
  show (Supply { piles=s }) = show $ sortBy (comparing $ cost . fst) s

-- A heuristic (function) is supplied by client code to determine what
-- action to perform based on the current game state. For now (a == Card)
-- is the only interesting instance of this type (i.e. buy and action
-- heuristics both produce a card).
type Heuristic a = Game -> IO a
type PickHeuristic b a = Game -> b -> IO a

data Player = Player 
  { name :: String, hand :: Pile, deck :: Pile, discardPile :: Pile
  , inPlay :: Pile, numBuys :: Int, numActions :: Int, amtMoney :: Int
  , actHeuristic   :: Heuristic (Maybe Card) -- Ask player what action to play
  , moneyHeuristic :: Heuristic (Maybe Card) -- Ask player what money card to play
  , buyHeuristic   :: Heuristic (Maybe Card) -- Ask player what card to buy
  , mayPick        :: PickHeuristic Card (Maybe Card) -- Ask player what card to pick e.g. during action
  , mustPick       :: PickHeuristic Card Card
  }

instance Eq Player where p1 == p2 = name p1 == name p2
instance Ord Player where p1 <= p2 = name p1 <= name p2

instance Show Player where
  show (Player { name = n, hand = h, deck = d, discardPile = dp, inPlay = ip,
               numBuys = nb, numActions = na, amtMoney = am}) =
    "    name   = " ++ show(n)  ++ "\n" ++
    "    hand   = " ++ show(h)  ++ "\n" ++ -- show them in enum order
    "    inPlay = " ++ show(ip) ++ "\n" ++ -- show them in the order they are played
    "    deck   = " ++ show(d)  ++ "\n" ++ -- show deck in actual order
    "    dscrd  = " ++ show(dp) ++ "\n" ++ -- show dscrd pile in actual order
    "    buys="     ++ show(nb) ++ ", actions=" ++ show(na) ++ ", money=" ++ show(am) ++ "\n"

data Game = Game
  { p1 :: Player, p2 :: Player, trash :: Pile
  , supply :: Supply, turn :: Int, maxTurns :: Int
  , doCardEffects :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => Card -> m ()
  }
-- negative maxTurns means unlimited turns

instance Eq Game where
  g == g' = all id $ [p1 g == p1 g', p2 g == p2 g', trash g == trash g', supply g == supply g', turn g == turn g']

instance Ord Game where
  g <= g' = turn g <= turn g'

instance Show Game where
  show (Game { p1 = p1, p2 = p2, trash = trash, supply = s, turn = turn }) = 
    "Player1:\n" ++ (show p1)    ++ "\n" ++
    "Player2:\n" ++ (show p2)    ++ "\n" ++
    "Trash: "    ++ (show trash) ++ "\n" ++ -- show trash in order trashed
    "Supply: "   ++ (show s)     ++ "\n" ++ -- show supply cards in order of cost
    "Turn #: "   ++ (show turn)  ++ "\n"

nullHeuristic :: Heuristic (Maybe a)
nullHeuristic = const (return Nothing)

defaultPile = Pile
  { cards      = []
  , visibleTo  = []
  , sortPileBy = Nothing
  }

defaultHand = defaultPile { sortPileBy = Just fromEnum }

-- Default player and game constructors:
defaultPlayer = Player
  { name="INVALID_PLAYER_NAME"
  , hand=defaultHand, inPlay=defaultPile
  , numActions=1, numBuys=1, amtMoney=0
  , deck = defaultPile { cards = (replicate 7 COPPER) ++ (replicate 3 ESTATE) }
  , discardPile = defaultPile
  , actHeuristic   = nullHeuristic -- default to always playing nothing
  , buyHeuristic   = nullHeuristic -- default to always buying nothing
  , moneyHeuristic = defaultMoneyHeuristic -- play all money by default
  , mayPick        = undefined
  , mustPick       = undefined
  }

defaultMoneyHeuristic :: Heuristic (Maybe Card)
defaultMoneyHeuristic = (\g -> return $ find isTreasure ((cards.hand.p1) g))

defaultSupply = Supply
  { piles=[]
  }

defaultGame = Game
  { p1=(defaultPlayer {name="p1"})
  , p2=(defaultPlayer {name="p2"})
  , trash=defaultPile
  , supply=defaultSupply
  , turn=0, maxTurns=100
  , doCardEffects=(\c -> return ())
  }

