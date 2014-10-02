module Dice where
import Infer

die2dist :: D -> P Int
die2dist (D 10) = equally [0..10]
die2dist (D sides) = equally [1..sides]
d2d = die2dist

classbag = normalize $ P [(d4,12), (d6,12), (d8,12), (d10,16), (d12,17), (d20,17)]
d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 

-- Produces the probability distribution (P Int) over the sum of two dice
sumDist :: D -> D -> P Int
sumDist d1 d2 = (pmap (\(a,b) -> (+) a b) (liftP (d2d d1) (d2d d2)))

-- Computes the probability of rolling a sum of sum' given dice d1 and d2
sumEQ :: D -> D -> Int -> Double
sumEQ d1 d2 sum' = probOf sum' (sumDist d1 d2)

-- (pmap (\(a,b) -> (+) a b) (liftP (d2d d1) (d2d d2)))

draw2 :: D -> D -> P D -> Double
draw2 d1 d2 bag = probOf (Pair d1 d2) (pmap (\(a,b) -> Pair a b) (liftP bag bag))

-- Probability that the given die is drawn exactly once
drawOnce :: D -> P D -> Double
drawOnce d bag = probOf True $ pmap (\(a,b) -> xor ((==) a d) ((==) b d)) (liftP bag bag)

-- Maps from bag space to pairs of dice space
allPairs :: P D -> P Pair
allPairs bag = pmap (\(dx,dy) -> Pair dx dy) (liftP bag bag)

-- Generates all possible (die #1, die #2, sum) triplings (tuples)
draw2bindx :: P D -> P (Pair, Int)
draw2bindx bag = bindx (allPairs bag) (\(Pair a b) -> sumDist a b)

-- Computes P(d1 && d2 && sum = s | bag)
draw2sum :: D -> D -> Int -> P D -> Double
draw2sum d1 d2 s bag = probOf (Pair d1 d2, s) (draw2bindx bag)

-- Computes P(d1 && d2 | sum = s)
draw2cndtnl :: D -> D -> Int -> P D -> Double
draw2cndtnl d1 d2 s bag =
  let dst2 = pfilter (\(_,s') -> (==) s s') (draw2bindx bag) -- filter out sums from dist (& renormalize inside pfilter)
  in probOf (Pair d1 d2, s) dst2

