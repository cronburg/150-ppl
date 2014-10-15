module InferHeader where
import Data.List
import Text.Printf

-- Data types
data D = D Int deriving (Show) -- dice
instance Eq D where D x == D y = x == y -- dice equality
data Pair = Pair D D deriving (Show) -- pair of dice
instance Eq Pair where Pair d1 d2 == Pair d3 d4 = (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)

data Column = LEFT | RIGHT deriving (Show,Eq)
data TallySheet = TS Int Int deriving (Show,Eq)

data P a = P [(a,Double)] deriving (Eq)
instance Show a => Show (P a) where
  show (P as) = ("P " ++  
    (show 
      (zip
        (map fst as) 
        (map (\x -> (printf "%.6f" x)::String) (map snd as)))))

data Bag a = Bag [a] 
instance Eq a => Eq (Bag a) where
  Bag [] == Bag [] = True
  Bag as == Bag [] = False
  Bag [] == Bag bs = False
  Bag (a:as) == Bag bs = (elem a bs) && ((Bag as) == (Bag (delete a bs)))

