import InferHeader
import Infer
import Dice
import Tally
import Gamble
import Text.Printf
import Data.List

problem1 :: (Int -> Bool) -> (Int -> Bool) -> (Int -> Bool) -> D -> P D -> Double
problem1 f1 f2 f3 die bag =
  let dst0 :: P [D] -- draw 3 dice
      dst0 = liftPn (replicate 3 bag)
      dst1 :: P ([D], [Int]) -- roll the three dice, keeping track of which dice you drew
      dst1 = bindx dst0 (\ds -> liftPn (map d2d ds))
      dst2 :: P ([D], [Int]) -- condition on the observation functions for each roll
      dst2 = pfilter (\(ds, r1:r2:r3:[]) -> (f1 r1) && (f2 r2) && (f3 r3)) dst1
      dst3 :: P Bool -- map to "at least one d8 drawn" space
      dst3 = pmap (\(ds, rs) -> elem die ds) dst2
  in probOf True dst3

{-
      dst1 :: P ([D],[[Int]]) -- P dist over lists of possible (draw,roll) tuples
      dst1 = bindx (map (replicate 3 
      pmap (\ds -> [(d


      dst1 :: P [(D, P Int)]
      dst1 = pmap (\ds -> [(d,d2d d) | d <- ds]) dst0
      dst2 :: P [Int]
      dst2 = bindx dst1 (\(p1:p2:p3:[]) -> liftPn [p1,p2,p3])
      dst3 :: P [Int] -- filter out rolls based on the 3 observation functions
      dst3 = pfilter (\(x:y:z:[]) -> (f1 x) && (f2 y) && (f3 z)) dst2
      dst4 :: P Bool
      dst4 = pmap (elem die) dst3
  in probOf True dst4
-}


{-
      dst1 :: P [D] -- filter out impossible dice to keep state space small
      dst1 = pfilter (\((D x):(D y):(D z):[]) -> (f1 x) && (f2 y) && (f3 z)) dst0
      dst2 :: P [Int]
      dst2 = liftPn (pmap d2d dst1)


      dst3 :: P [(D,P Int)]
      dst3 = pmap (\(((D d1),p1):((D d2),p2):((D d3),p3):[]) ->
             (pfilter ((==) 7) p1)
             (pfilter ((==) 11) p2)
             (pfilter (\n -> (mod n 4) == 0) p3)
             ) dst2

      dst :: P Bool
      dst = pmap (elem die) dst???
  in probOf True dst
-}

{-  let dst0 :: P [D] -- P dist over possible draws of 3 dice
      dst0 = liftPn (replicate 3 bag)
      dst1 :: P [(D,P Int)] -- P dist over possible rolls of 3 dice
      dst1 = pmap (\ds -> [(d, d2d d) | d <- ds]) dst0
      dst2 :: P [D] -- filter out rolls based on observation functions
      dst2 = pmap (\dps -> map (uncurry pfilter) (zip [f1,f2,f3] (snd $ unzip dps))) dst1
-}

{-
      dst3 = pmap (\(p1:p2:p3:[]) -> pfilter (\i -> i == r2) p2) dst2
      dst2 = pmap (\(p1:p2:p3:[]) -> pfilter f3 p1) dst2



      dst1 :: P [D]
      dst1 = pfilter (\((D x):(D y):(D z)) -> (x >= r1) && (y >= r2))
      dst2 :: P [P Int]
      dst2 = pmap d2d dst1
      dst3 :: P [P Int]
      dst3 = pfilter (\(p1:p2:p3:[]) -> pfilter (\) p3) dst2


      dst1 :: P [P Int]
      dst1 = pmap d2d dst0
      dst2 :: P [P Int] -- filter out impossible die draws for the first two dice
      dst2 = pfilter (\(p1:p2:p3:[]) -> ((probOf r1 p1) > 0) && ((probOf r2 p2) > 0)) dst1
      dst3 :: P [P Int] -- filter out impossible 
      dst3 = pfilter (\(p1:p2:p3:[]) -> 
      
      
      
      
      dst2 :: P [P Int]
      dst3 :: P [P Int]
      dst3 = pfilter (\(p1:p2:p3:[]) -> 
      dst4 :: P Bool
      dst4 = pmap (elem die) dst3
  in probOf True dst2
-}

main = do
  printf "-------- New Problems ----------\n"
  printf "1) P(d8 in [r1,r2,r3] | r1 = 7, r2 = 11, (r3 %% 4) == 0 for some r3) = %.4f %%\n" $ 100.0 *
         (problem1 ((==) 7) ((==) 11) (\n -> (mod n 4) == 0) d8 classbag)
         
         --(mod n 4) == 0) d8 classbag)
{-
  printf "-------- Dice Problems ---------\n"
  printf "A.1) P dist of a D6               = %s\n" (show $ d2d d6)
  printf "A.2) P dist of a D12              = %s\n" (show $ d2d d12)
  printf "B) P(sum = 11 | d6 && d12)        = %7.4f %%\n" $ 100.0 * (sumEQ d6 d12 11)
  printf "C.1) P(d6 && d12 | bag)           = %7.4f %%\n" $ 100.0 * (draw2 d6 d12 classbag)
  printf "C.2) P(d20 && !d20 | bag)         = %7.4f %%\n" $ 100.0 * (drawOnce d20 classbag)
  printf "D) P(d6 && d12 && sum = 11 | bag) = %7.4f %%\n" $ 100.0 * (draw2sum d6 d12 11 classbag)
  printf "E) P(d6 && d12 | sum = 11)        = %7.4f %%\n" $ 100.0 * (draw2cndtnl d6 d12 11 classbag)
  printf "-------- Tally Problems --------\n"
  
  --print (singleTally d4 d4 2 8)
  printf "F) P(d4 && d4 && LHS = 3 marks)   = %.4f %%\n"
         ((*) 100 (jointTally d4 d4 (TS 27 3) classbag 8))
  printf "G) Expected(RIGHT marks)          = %.2f\n"
         (expectedMarks classbag RIGHT 30 8)

  printf "-------- Gambling Problems -----\n"
  printf "H) Guesser has better expectation (see part I)\n"
  let payout30 = (*) 1000 (expPayout classbag (Win 1.00 0.50 0.25 (-0.10)) 30 8)
  printf "I) Guesser wins after 1,000 games : $ %.2f\n" (payout30)
  let payout50 = (*) 1000 (expPayout classbag (Win 1.00 0.50 0.25 (-0.10)) 50 8)
  printf "J) Thrower should charge guesser  : $ %.2f   ( = %.2f - %.2f )\n"
         (payout50 - payout30) payout50 payout30
  let cutoff10 = (*) 1000 (expPayout classbag (Win 1.00 0.50 0.25 (-0.10)) 30 10)
  printf "K) Guesser should at most pay     : $ %.2f   ( = %.2f - %.2f )\n"
         (cutoff10 - payout30) cutoff10 payout30
  printf "L) Best split for guesser         = %d-%d\n" (bestCutoff - 1) bestCutoff
  print (allCutoffs)
-}

