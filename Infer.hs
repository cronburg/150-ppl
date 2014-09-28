module Infer where
import Data.List
import Text.Printf

-- Data types
data D = D Int deriving (Show) -- dice
instance Eq D where D x == D y = x == y -- dice equality
data Pair = Pair D D deriving (Show) -- pair of dice
instance Eq Pair where Pair d1 d2 == Pair d3 d4 = (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)

data P a = P [(a,Double)] deriving (Eq)
instance Show a => Show (P a) where
  show (P as) = ("P " ++ 
    (show 
      (zip
        (map fst as)
        (map (\x -> (printf "%.6f" x)::String) (map snd as)))))

-- Macros
rTF :: (Real a, Fractional b) => a -> b
rTF a = realToFrac a

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && (not (p && q))

count :: Eq a => a -> [a] -> Int
count a as = length $ elemIndices a as

-- Gets the domain (list of a's) for which the P a is non-zero
support :: P a -> [a]
support (P xs) = map fst xs

-- Creates a distribution where each element in xs is equally likely
equally :: Eq a => [a] -> P a
equally xs = P (zip xs (replicate (length xs) ((/) 1.0 (rTF (length xs)))))

-- Get the probability of 'a' occuring in 'P a'
probOf :: Eq a => a -> P a -> Double
probOf a (P as) = case (lookup a as) of
  Just pa -> pa
  Nothing -> 0.0

-- Bind two distributions into one over all possible pairings
bind :: P a -> P b -> P (a,b)
bind (P as) (P bs) = P [((a,b),pa*pb) | (a,pa) <- as, (b,pb) <- bs]

-- Bind n distributions into one over all possible groupings
bindn :: Eq a => [P a] -> P [a]
bindn (d0:[]) = pmap (\a -> [a]) d0 -- convert a's to list of a's
bindn (d0:d1:[]) = pmap (\(a0,a1) -> [a0,a1]) (bind d0 d1) -- convert (a0,a1)'s to [a0,a1]'s
-- recursively bind and convert (a0,(a1,(...,(an)))) into [a0,a1,...,an]
bindn (d0:ds) = pmap (\(a,as) -> a:as) (bind d0 (bindn ds))

-- A Bag is a an unordered list (order doesn't matter) which allows duplicates
data Bag a = Bag [a] 
instance Eq a => Eq (Bag a) where
  Bag [] == Bag [] = True
  Bag as == Bag [] = False
  Bag [] == Bag bs = False
  Bag (a:as) == Bag bs = (elem a bs) && ((Bag as) == (Bag (delete a bs)))

-- Bind n distributions into one over all possible groupings (where order doesn't matter)
bindnBag2 :: Eq a => [P a] -> P (Bag a)
bindnBag2 (d0:[]) = pmap (\a -> Bag [a]) d0 -- convert a's to list of a's
bindnBag2 (d0:d1:[]) = pmap (\(a0,a1) -> Bag [a0,a1]) (bind d0 d1) -- convert (a0,a1)'s to [a0,a1]'s
-- recursively bind and convert (a0,(a1,(...,(an)))) into [a0,a1,...,an]
bindnBag2 (d0:ds) = pmap (\(a,Bag as) -> Bag (a:as)) (bind d0 (bindnBag2 ds))

bindnBag :: Eq a => [P a] -> P [a]
bindnBag ds = pmap (\(Bag as) -> as) (bindnBag2 ds)

-- Change domain without changing probabilities
pmap :: Eq b => (a -> b) -> P a -> P b
pmap f (P as) = regroup $ P [(f a, pa) | (a,pa) <- as]

-- Normalizes the probabilities (sum to 1.0)
normalize :: P a -> P a
normalize (P as) = P [(a,(/) pa $ sum $ map snd as) | (a,pa) <- as]

--zip (map fst as) (map (\ pa -> (/) pa (sum (map snd as))) (map snd as))

-- Zero out the probability of 'a' items not matching the criterion function
pfilter :: (a -> Bool) -> P a -> P a
pfilter f (P as) = normalize $ P (filter (\ (a,_) -> f a) as)

-- pfilter without normalizing (i.e. dfilter == Distribution Filter)
dfilter :: (a -> Bool) -> P a -> P a
dfilter f (P as) = P (filter (\ (a,_) -> f a) as)

-- TODO: actually regroup things
regroup :: Eq a => P a -> P a
regroup (P as) = P [(a, sum $ map snd (filter (\(x,px) -> (==) a x) as)) | a <- (nub $ map fst as)]

-- Bind a distribution with a function over its support
-- i.e. the "joint" probability for each pairing (a,b)
bindx :: (Eq a, Eq b) => P a -> (a -> P b) -> P (a,b)
bindx (P as) f =
  let --pbs :: [(P b,Double)]
      P pbs = pmap f (P as)
      -- zp :: [((a,Double),(P b,Double))]
      zp = zip as pbs
  in P $ concat [ [ ((a,b),pa*pb) | (b,pb) <- bs] | ((a,pa),(P bs,pa_)) <- zp]

