import Infer
import Text.Printf
import Data.List

die2dist :: D -> P Int
die2dist = (\(D sides) -> equally [1..sides])
d2d = die2dist

classbag = normalize $ P [(d4,12), (d6,12), (d8,12), (d10,16), (d12,17), (d20,17)]
d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 

-- Produces the probability distribution (P Int) over the sum of two dice
sumDist :: D -> D -> P Int
sumDist d1 d2 = (pmap (\(a,b) -> (+) a b) (bind (d2d d1) (d2d d2)))

-- Computes the probability of rolling a sum of sum' given dice d1 and d2
sumEQ :: D -> D -> Int -> Double
sumEQ d1 d2 sum' = probOf sum' (sumDist d1 d2)

-- (pmap (\(a,b) -> (+) a b) (bind (d2d d1) (d2d d2)))

draw2 :: D -> D -> P D -> Double
draw2 d1 d2 bag = probOf (Pair d1 d2) (pmap (\(a,b) -> Pair a b) (bind bag bag))

-- Probability that the given die is drawn exactly once
drawOnce :: D -> P D -> Double
drawOnce d bag = probOf True $ pmap (\(a,b) -> xor ((==) a d) ((==) b d)) (bind bag bag)

-- Generates all possible (die #1, die #2, sum) triplings (tuples)
draw2bindx :: P D -> P (Pair, Int)
draw2bindx bag =
  let allPairs :: P Pair
      allPairs = pmap (\(dx,dy) -> Pair dx dy) (bind bag bag)
  in bindx allPairs (\(Pair a b) -> sumDist a b)

-- Computes P(d1 && d2 && sum = s | bag)
draw2sum :: D -> D -> Int -> P D -> Double
draw2sum d1 d2 s bag = probOf (Pair d1 d2, s) (draw2bindx bag)
  
-- Computes P(d1 && d2 | sum = s)
draw2cndtnl :: D -> D -> Int -> P D -> Double
draw2cndtnl d1 d2 s bag =
  let dst2 = pfilter (\(_,s') -> (==) s s') (draw2bindx bag) -- filter out sums from dist (& renormalize inside pfilter)
  in probOf (Pair d1 d2, s) dst2

-- Generates the P Dist over the domain of the # of tallies in the left column
-- Don't need to specify tallies on RHS because RHS = throws - LHS. If you really
-- need RHS, you can pmap your distribution to convert to RHS.
singleTally :: D -> D -> Int -> Int -> P Int
singleTally d1 d2 throws cutoff =
  let dst :: P Int
      dst = (sumDist d1 d2) -- dist from (sum -> probability)
      dst2 :: P Bool
      dst2 = pmap (\s -> s < cutoff) dst -- dist from ((sum < cutoff) -> prob)
      dst3 :: P [Bool]
      dst3 = bindnBag $ replicate throws dst2 -- bind 30 distributions together, creating P (Bool,Bool,Bool,...,Bool)
      dst4 = pmap (\bs -> count True bs) dst3
      --foldl (\dist1 dist2 -> ) dst2 (replicate (throws-1) dst2)
  --[1..throws]
  in dst4

-- P(d4 && d4 && LEFT tallies = n | throws, LEFT < cutoff, bag)
obsvTlly :: D -> D -> Int -> P D -> Int -> Int -> Double
obsvTlly d1 d2 n bag throws cutoff =
  let dst :: P (Pair, Int)
      dst = draw2bindx bag
      dst2 :: P (Pair, Bool)
      dst2 = pmap (\(ab,s) -> (ab,s < cutoff)) dst -- pmap to P dist over (die,die,LEFT | RIGHT) where LEFT == True, RIGHT == False
      dst3 :: P (Pair, Bool)
      dst3 = dfilter (\(ab,s) -> s) dst2 -- filter out RIGHT column from distribution
      dst4 :: P Pair
      dst4 = pmap (\(ab,s) -> ab) dst3 -- unmap the sum < cutoff column
      dst5 :: P (Pair, Int)
      dst5 = bindx dst4 (\(Pair dx dy) -> singleTally dx dy throws cutoff) -- P dist over (Pair, #Tallies on LHS)
  in probOf (Pair d1 d2, n) dst5

main = do
  printf "A.1) P dist of a D6  = %s\n" (show $ d2d d6)
  printf "A.2) P dist of a D12 = %s\n" (show $ d2d d12)
  printf "B) P(sum = 11 | d6 && d12) = %.6f\n" (sumEQ d6 d12 11)
  printf "C.1) P(d6 && d12 | bag) = %.6f\n"  (draw2 d6 d12 classbag)
  printf "C.2) P(d20 && !d20 | bag) = %.6f\n" (drawOnce d20 classbag)
  printf "D) P(d6 && d12 && sum = 11 | bag) = %.6f\n" (draw2sum d6 d12 11 classbag)
  printf "E) P(d6 && d12 | sum = 11) = %.6f\n" (draw2cndtnl d6 d12 11 classbag)
  print  "-------- Tally Problems --------"
  --print (singleTally d4 d4 30 8)
  printf "F) P(d4 && d4 && LHS = 3 marks) = %.8e\n" (obsvTlly d4 d4 3 classbag 30 8)

  --print (pfilter (\a,b -> a+b) (show $ bind (d2d d6) (d2d d12))
  -- printf "B) P(sum = 11 | d6 && d12) = %.4f"

