import Infer
import Text.Printf

main = do
  let d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 
  let bag = (Bag [(12,d4), (12,d6), (12,d8), (16,d10), (17,d12), (17,d20)])
  printf "F) "

