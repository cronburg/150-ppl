import Infer
import Dice
import Tally
import Gamble
import Text.Printf
import Data.List

main = do
  printf "-------- Dice Problems ---------\n"
  printf "A.1) P dist of a D6  = %s\n" (show $ d2d d6)
  printf "A.2) P dist of a D12 = %s\n" (show $ d2d d12)
  printf "B) P(sum = 11 | d6 && d12) = %.6f\n" (sumEQ d6 d12 11)
  printf "C.1) P(d6 && d12 | bag) = %.6f\n"  (draw2 d6 d12 classbag)
  printf "C.2) P(d20 && !d20 | bag) = %.6f\n" (drawOnce d20 classbag)
  printf "D) P(d6 && d12 && sum = 11 | bag) = %.6f\n" (draw2sum d6 d12 11 classbag)
  printf "E) P(d6 && d12 | sum = 11) = %.6f\n" (draw2cndtnl d6 d12 11 classbag)
  printf "-------- Tally Problems --------\n"
  ----print (singleTally d4 d4 30 8)
  printf "F) P(d4 && d4 && LHS = 3 marks) = %.8e\n"
    (jointTally d4 d4 (TS 27 3) classbag 8)
  --printf "G) Expected number of marks in RIGHT column = %.2f\n" (avgRightMarks classbag 30 8)
  
  
  --printf "-------- Gambling Problems -----\n"
  --print (expPayout classbag 30 8 1.00 0.50 0.25 (-0.10))
  
  --printf "H) Expected payout for Guesser after one game = $ %.4f\n" (expPayout classbag 30 8 1.00 0.50 0.25 (0 - 0.10))
  --printf "I) Expected payout for Guesser after one game = $ %.4f\n" (expPayout classbag 50 8 1.00 0.50 0.25 (0 - 0.10))
  
  --print $ weight (tallyDist classbag 1 8)
  
  --print (expPayoutDist 3 classbag 30 8 100.00 0.50 0.25 (-0.10))

  --print (pfilter (\a,b -> a+b) (show $ bind (d2d d6) (d2d d12))
  -- printf "B) P(sum = 11 | d6 && d12) = %.4f"

