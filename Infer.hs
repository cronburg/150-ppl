module Infer where
import Data.List

-- Data types
data D = D Int deriving( Show )
instance Eq D where D x == D y = x == y

-- An unordered pair of dice (order doesn't matter for comparison)
data Pair = Pair D D deriving( Show )
instance Eq Pair where Pair d1 d2 == Pair d3 d4 = (d1 == d3 && d2 == d4) || (d1 == d4 && d2 == d3)

data P a =  
    SingleDie D
  | PDist [(a,Float)]
  | TwoDie { d1 :: D, d2 :: D }
  deriving( Show )

newtype Bag = Bag [(Int,D)] deriving( Show )

-- Macros
rTF :: (Real a, Fractional b) => a -> b
rTF a = realToFrac a

-- Part (A)
equally :: [a] -> P a
equally xs = PDist
  (zip
    xs
    (replicate (length xs) ((/) 1.0 (realToFrac (length xs)))))

-- Part (B)
throw :: D -> P Int
throw d = (SingleDie d)

-- Count # of instances of x in list of xs
count :: (Eq a) => a -> [a] -> Int
count x xs = length (filter (\x' -> x' == x) xs)

-- P(Sum = sum | D1 = d1, D2 = d2)
throw2 :: D -> D -> Int -> Float
throw2 (D d1) (D d2) sum =
  let allsums = (concat (map (\y -> (map (+y) [1..d1])) [1..d2])) in
  ((/) (realToFrac (count sum allsums)) (realToFrac (length allsums)))

-- Part (C)
draw :: D -> Bag -> Float
draw d (Bag xs)
  | xs == [] = 0.0
  | otherwise = 
    ((/) c s)
    where
      s = realToFrac (sum (map fst xs))  -- number of dice in the bag
      dice = (map snd xs)                -- list of dice
      Just i = elemIndex d dice          -- index of the die in the bag
      c = realToFrac (fst (xs !! i))     -- number dice with d-sides in the bag
  

-- Probability of drawing a d1-sided die and a d2-sided die from the given bag
-- P(D1 = d1 and D2 = d2) = P(D1 = d1 | D2 = d2) * P(D2 = d2)
-- Drawing with replacement, so draws from bag are independent
draw2 :: D -> D -> Bag -> Float
draw2 d1 d2 bag =
  ((*) (draw d1 bag) (draw d2 bag))

-- Part (D)
joint :: D -> D -> Int -> Bag -> Float
joint d1 d2 s bag = (draw2 d1 d2 bag) * (throw2 d1 d2 s)

-- Computes the number of ways an m-sided and an n-sided die can sum to s
countSums :: Int -> Int -> Int -> Int
countSums m n s
  | m > n = countSums n m s  -- re-order terms such that n >= m
  | (m >= s) && (n >= s) = s
  | (m < s) && (n >= s) = m
  | n + m < s = 0
  | (m < s) && (n < s) = n+m-s+1
  | otherwise = error "invalid countSums input?"

-- Part (E)
cndtnl :: D -> D -> Int -> Bag -> Float
cndtnl (D d1) (D d2) sum (Bag bag) =
  let dice = map snd bag
      pairs = [Pair x y | x <- dice, y <- dice]
      f1 = (filter (\ (Pair (D a) (D b)) -> ((a+b) >= sum)) pairs) -- filter out pairs which can't add to sum
      f2 = [(count x f1,x) | x <- (nub f1)] -- compress duplicate pairs
      f3 = [ (n,countSums a b sum) | (n,Pair (D a) (D b)) <- f2]
      denom = foldr (+) 0 (map (\ (a,b) -> a*b) f3) -- # of ways possible to roll sum given bag
      numer = countSums d1 d2 sum -- # of ways to roll sum given d1 and d2
  in (rTF numer) / (rTF denom)

--countTally :: Int -> Int -> Int -> Int

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k

-- Computes the number of ways an m-sided and an n-sided die can sum to greater than s
countGTSums :: Int -> Int -> Int -> Int
countGTSums m n s
  | n + m < s = 0
  | m > n = countGTSums n m s -- re-order terms such that n >= m
  | (m >= s) && (n >= s) = (n-2*s+m)*m - (countSums m n s)
  | (m < s) && (n >= s) = (n-s-1)*m + (div (m*(m-1)) 2)
  | (m < s) && (n < s) = div (((^) (n+m-s+1) 2) - (countSums m n s)) 2

{-|
-- Part (F)
RTally :: D -> D -> Int -> Int -> Bag -> Float
RTally (D d1) (D d2) tallies trials bag =
  let
    bin_coeff = (choose trials tallies)
    p1 = (^^) (countSums ) tallies
  in
    returnval

--RTally :: D -> D -> Int -> Int -> Bag -> Float
--RTally d1 d2 tallies trials =
--  let pdice = (draw2 d1 d2 bag) -}

