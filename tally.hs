import Infer2
import Text.Printf

main = do
  let d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 
  let bag = (P [(d4,0.2),(d6,0.6),(d8,0.2)])
  let die2dist = (\(D sides) -> equally [1..sides])
  print $ bindx bag die2dist
  printf "probOf d6 bag = %.4f\n" (probOf d6 bag)
  printf "probOf d12 bag = %.4f\n" (probOf d12 bag)
  print $ pfilter (\ x -> x == d6) bag

  --let bag = (Bag [(12,d4), (12,d6), (12,d8), (16,d10), (17,d12), (17,d20)])
  --printf "F) "

