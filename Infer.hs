module Infer where
import Data.List
import InferHeader

-- Macros
rTF :: (Real a, Fractional b) => a -> b
rTF a = realToFrac a
fI :: (Integral a, Num b) => a -> b
fI a = fromIntegral a

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
liftP :: P a -> P b -> P (a,b)
liftP (P as) (P bs) = P [((a,b),pa*pb) | (a,pa) <- as, (b,pb) <- bs]

-- Bind n distributions into one over all possible groupings
liftPn :: Eq a => [P a] -> P [a]
liftPn (d0:[]) = pmap (\a -> [a]) d0 -- convert a's to list of a's
liftPn (d0:d1:[]) = pmap (\(a0,a1) -> [a0,a1]) (liftP d0 d1) -- convert (a0,a1)'s to [a0,a1]'s
-- recursively bind and convert (a0,(a1,(...,(an)))) into [a0,a1,...,an]
liftPn (d0:ds) = pmap (\(a,as) -> a:as) (liftP d0 (liftPn ds))

-- Bind n distributions into one over all possible groupings (where order doesn't matter)
liftPnBag2 :: Eq a => [P a] -> P (Bag a)
liftPnBag2 (d0:[]) = pmap (\a -> Bag [a]) d0 -- convert a's to list of a's
liftPnBag2 (d0:d1:[]) = pmap (\(a0,a1) -> Bag [a0,a1]) (liftP d0 d1) -- convert (a0,a1)'s to [a0,a1]'s
-- recursively liftP and convert (a0,(a1,(...,(an)))) into [a0,a1,...,an]
liftPnBag2 (d0:ds) = pmap (\(a,Bag as) -> Bag (a:as)) (liftP d0 (liftPnBag2 ds))

-- Lift a list of P Dists into a P Dist over bags over the original domain
liftPnBag :: Eq a => [P a] -> P [a]
liftPnBag ds = pmap (\(Bag as) -> as) (liftPnBag2 ds)

-- Change domain without changing probabilities
pmap :: Eq b => (a -> b) -> P a -> P b
pmap f (P as) = regroup $ P [(f a, pa) | (a,pa) <- as]

-- Normalizes the probabilities (sum to 1.0)
normalize :: P a -> P a
normalize (P as) = P [(a,(/) pa $ sum $ map snd as) | (a,pa) <- as]

-- The total 'weight' of a distribution, i.e. what the individual weights / probabilities
-- sum to. A normalized probability distribution has a weight of 1.0
weight :: P a -> Double
weight (P as) = sum (map snd as)

-- Whether or not the given distribution is normalized
isnorm :: P a -> Bool
isnorm pas = (==) 1.0 (weight pas)

--zip (map fst as) (map (\ pa -> (/) pa (sum (map snd as))) (map snd as))

-- Zero out the probability of 'a' items not matching the criterion function
pfilter :: (a -> Bool) -> P a -> P a
pfilter f (P as) = normalize $ P (filter (\ (a,_) -> f a) as)

-- pfilter without normalizing (i.e. dfilter == Distribution Filter)
dfilter :: (a -> Bool) -> P a -> P a
dfilter f (P as) = P (filter (\ (a,_) -> f a) as)

-- TODO: actually regroup things
-- use Ord a instead of Eq.
regroup :: Eq a => P a -> P a
regroup (P as) = P [(a, sum $ map snd (filter (\(a',pa') -> (==) a a') as)) | a <- (nub $ map fst as)]

-- Bind a distribution with a function over its support
-- i.e. the "joint" probability for each pairing (a,b)
bindx :: (Eq a, Eq b) => P a -> (a -> P b) -> P (a,b)
bindx (P as) f =
  let --pbs :: [(P b,Double)]
      P pbs = pmap f (P as)
      -- zp :: [((a,Double),(P b,Double))]
      zp = zip as pbs
  --in P [ ((a,b),pa*pb) | (a,pa) <- as, (P bs,pb) <- pbs]
  in P $ concat [ [ ((a,b),pa*pb) | (b,pb) <- bs] | ((a,pa),(P bs,pa_)) <- zp]

-- Computes the expected value of the given numeric P dist
expected :: Real a => P a -> Double
expected (P as) = foldl (\b (a,pa) -> b + ((rTF a) * pa)) 0.0 as

-- Determines the nth most probable (value,prob) pair in the distribution
mostProb :: Eq a => Int -> P a -> (a,Double)
mostProb 0 (P as) =
  let f = (\(a1,pa1) (a2,pa2) -> if (pa1 > pa2) then (a1,pa1) else (a2,pa2))
  in foldl f (head as) (tail as)
mostProb n (P as) = mostProb (n-1) $ P $ delete (mostProb 0 (P as)) as

