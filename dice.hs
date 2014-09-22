import Infer
import Text.Printf

{-|
main = do
  let d2 = D 2; d3 = D 3; d4 = D 4; d5 = D 5; d6 = D 6; d12 = D 12; d20 = D 20;
  let b1 = (Bag [(5,d4), (2,d5), (3,d6), (3,d12)])
  let b2 = (Bag [(5,d20), (5,d6)])
  let b3 = (Bag [(1,d2), (1,d3), (1,d4), (1,d5), (1,d6)])
  print (equally [1.0..6.0])
  print (throw (D 6)) -- (A)
  print (throw (D 12)) -- (A)
  --print (count 1 [1,2,3,1,1])
  print (throw2 (D 6) (D 12) 11) -- (B)
  print (draw (D 12) b1) -- (C.1)
  print ((draw d20 b2) * (1.0 - (draw d20 b2))) -- (C.2)
  print (joint d6 d12 11 b1) -- (D)
  print (cndtnl d3 d5 6 b3) 
  print (cndtnl d3 d3 6 (Bag [(1,d3)]))
  print (cndtnl d3 d3 6 (Bag [(1,d3),(1,d4)])) -}

main = do
  let d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 
  let bag = (Bag [(12,d4), (12,d6), (12,d8), (16,d10), (17,d12), (17,d20)])
  print (throw2 (D 6) (D 12) 11) -- (B)
  printf "C.1) Prob of drawing (d6,d12) = %.4f\n" (draw2 d6 d12 bag)
  printf "C.2) P(d20,!d20) = %.4f\n" ((draw d20 bag) * (1.0 - (draw d20 bag)))
  printf "D) P(d6 && d12 && s=11) = %.4f\n" (joint d6 d12 11 bag)
  printf "E) P(d6 && d12 |  s=11) = %.4f\n" (cndtnl d6 d12 11 bag)
  --print (cndtnl d6 d12 11 bag)

