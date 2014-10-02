import Infer
import Dice
import Tally
import Gamble
import Text.Printf
import Data.List

allCutoffs :: [(Int,Double)]
allCutoffs = [(cutoff,(*) 1000 (expPayout classbag (Win 1.00 0.50 0.25 (-0.10)) 30 cutoff)) | cutoff <- [3..27]]

bestCutoff :: Int
bestCutoff = fst (foldl (\(b,eb) (a,ea) -> if ea > eb then (a,ea) else (b,eb))
                         (head allCutoffs) (tail allCutoffs))

main = do
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
  printf "L) Best split for guesser         = %d" (bestCutoff)
  --print (allCutoffs)

