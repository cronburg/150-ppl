{-# LANGUAGE DeriveDataTypeable,RankNTypes,FlexibleInstances #-}
module Game.DeckBuild.Dominion.Types where
import Data.Typeable
import Control.Monad.State

-- Kingdom Card class type
-- (Eq a, Ord a, Typeable a, Show a, Enum a) => 

data Card =
     -- Non-Kingdom cards:
       COPPER | SILVER | GOLD | ESTATE | DUCHY | PROVINCE
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

nkCards = [COPPER,SILVER,GOLD,ESTATE,DUCHY,PROVINCE]
bCards  = [CELLAR, CHAPEL, MOAT, CHANCELLOR, VILLAGE, WOODCUTTER
         , WORKSHOP, BUREAUCRAT, FEAST, GARDENS, MILITIA, MONEYLENDER
         , REMODEL, SMITHY, SPY, THIEF, THRONEROOM, COUNCILROOM
         , FESTIVAL, LABORATORY, LIBRARY, MARKET, MINE, WITCH
         , ADVENTURER]
iCards  = [COURTYARD, PAWN, SECRETCHAMBER, GREATHALL, MASQUERADE, SHANTYTOWN
         , STEWARD, SWINDLER, WISHINGWELL, BARON, BRIDGE, CONSPIRATOR
         , COPPERSMITH, IRONWORKS, MININGVILLAGE, SCOUT, DUKE, MINION
         , SABOTEUR, TORTURER, TRADINGPOST, TRIBUTE, UPGRADE, HAREM
         , NOBLES]

isKingdom :: Card -> Bool
isKingdom c = any (elem c) [bCards,iCards]

isTreasure :: Card -> Bool
isTreasure c = elem c [COPPER,SILVER,GOLD]

-- TODO: setup / write SYB or TemplateHaskell to auto-create the above data type definitions

-- Data & type definitions:
type Pile = [Card]
type Piles = [(Card,Int)]

data Player = Player { name :: String, hand :: Pile, deck :: Pile, discardPile :: Pile,
                       inPlay :: Pile, numBuys :: Int, numActions :: Int, amtMoney :: Int,
                       actHeuristic :: Game -> Maybe Card, -- Ask player what action to play
                       buyHeuristic :: Game -> Maybe Card  -- Ask player what card to buy
                     }

instance Eq Player where p1 == p2 = name p1 == name p2
instance Ord Player where p1 <= p2 = name p1 <= name p2

instance Show Player where
    show (Player { name = n, hand = h, deck = d, discardPile = dp, inPlay = ip,
                   numBuys = nb, numActions = na, amtMoney = am}) =
        "    name   = " ++ show(n) ++ "\n" ++
        "    hand   = " ++ show(h) ++ "\n" ++
        "    inPlay = " ++ show(ip) ++ "\n" ++
        "    deck   = " ++ show(d) ++ "\n" ++
        "    dscrd  = " ++ show(dp) ++ "\n" ++
        "    buys=" ++ show(nb) ++ ", actions=" ++ show(na) ++ ", money=" ++ show(am) ++ "\n"

data Game = Game { p1 :: Player, p2 :: Player, trash :: Pile,
                   supply :: [(Card,Int)], turn :: Int }
                   --rng :: IO StdGen }

instance Eq Game where
    g == g' = all id $ [p1 g == p1 g', p2 g == p2 g', trash g == trash g', supply g == supply g', turn g == turn g']

instance Ord Game where
    g <= g' = turn g <= turn g'

instance Show Game where
    show (Game { p1 = p1, p2 = p2, trash = trash, supply = s, turn = turn }) = 
        "Player1:\n" ++ (show p1) ++ "\n" ++
        "Player2:\n" ++ (show p2) ++ "\n" ++
        "Trash: " ++ (show trash) ++ "\n" ++
        "Supply: " ++ (show s) ++ "\n" ++
        "Turn #: " ++ (show turn) ++ "\n"

-- Default player and game constructors:
defaultPlayer = Player  { name="INVALID_PLAYER_NAME",
                          hand=[], inPlay=[],
                          numActions=1, numBuys=1, amtMoney=0,
                          deck=(replicate 7 COPPER) ++ (replicate 3 ESTATE),
                          discardPile=[],
                          actHeuristic = const Nothing, -- default to always playing nothing
                          buyHeuristic = const Nothing  -- default to always buying nothing
                        }

defaultGame = Game { p1=(defaultPlayer {name="p1"}), p2=(defaultPlayer {name="p2"}),
                     trash=[], supply=[], turn=0 }

