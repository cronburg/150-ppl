import Infer
import Text.Printf

main = do
  let d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 
  let bag = normalize $ P [(d4,12), (d6,12), (d8,12), (d10,16), (d12,17), (d20,17)]
  
  let die2dist = (\(D sides) -> equally [1..sides])
  print $ bindx bag die2dist
  printf "probOf d6 bag = %.4f\n" (probOf d6 bag)
  printf "probOf d12 bag = %.4f\n" (probOf d12 bag)
  print $ pfilter (\ x -> x == d6) bag

  --let bag = (Bag [(12,d4), (12,d6), (12,d8), (16,d10), (17,d12), (17,d20)])
  --printf "F) "

