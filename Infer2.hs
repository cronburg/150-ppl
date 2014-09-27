module Infer2 where
import Data.List

-- Data types
data D = D Int deriving (Show) -- dice
instance Eq D where D x == D y = x == y -- dice equality
data Pair = Pair D D deriving (Show) -- pair of dice
instance Eq Pair where Pair d1 d2 == Pair d3 d4 = (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)

data P a = P [(a,Double)] deriving (Show)

-- Macro
rTF :: (Real a, Fractional b) => a -> b
rTF a = realToFrac a

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

-- Change domain without changing probabilities
pmap :: (a -> b) -> P a -> P b
pmap f (P as) = P [(f a, pa) | (a,pa) <- as]

-- Normalizes the probabilities (sum to 1.0)
normalize :: P a -> P a
normalize (P as) = P [(a,(/) pa $ sum $ map snd as) | (a,pa) <- as]

--zip (map fst as) (map (\ pa -> (/) pa (sum (map snd as))) (map snd as))

-- Zero out the probability of 'a' items not matching the criterion function
pfilter :: (a -> Bool) -> P a -> P a
pfilter f (P as) = normalize $ P (filter (\ (a,_) -> f a) as)

-- TODO: actually regroup things
--regroup :: P a -> P a
--regroup (P as) = P as

-- Bind a distribution with a function over its support
-- i.e. the "joint" probability for each pairing (a,b)
bindx :: (Eq a, Eq b) => P a -> (a -> P b) -> P (a,b)
bindx (P as) f =
  let --pbs :: [(P b,Double)]
      P pbs = pmap f (P as)
      -- zp :: [((a,Double),(P b,Double))]
      zp = zip as pbs
  in P $ concat [ [ ((a,b),pa*pb) | (b,pb) <- bs] | ((a,pa),(P bs,pa_)) <- zp]

