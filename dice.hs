import Infer
import Text.Printf

die2dist :: D -> P Int
die2dist = (\(D sides) -> equally [1..sides])
d2d = die2dist

bag = normalize $ P [(d4,12), (d6,12), (d8,12), (d10,16), (d12,17), (d20,17)]
d4 = D 4; d6 = D 6; d8 = D 8; d10 = D 10; d12 = D 12; d20 = D 20; 

sumGT :: D -> D -> Int -> Double
sumGT d1 d2 sum' = probOf sum' $ (pmap (\(a,b) -> (+) a b) (bind (d2d d1) (d2d d2)))

draw2 :: D -> D -> P D -> Double
draw2 d1 d2 bag = probOf (Pair d6 d12) (pmap (\(a,b) -> Pair a b) (bind bag bag))

-- Probability that the given die is drawn exactly once
drawOnce :: D -> P D -> Double
drawOnce d bag = probOf True $ pmap (\(a,b) -> xor ((==) a d) ((==) b d)) (bind bag bag)

main = do
  printf "A.1) P dist of a D6  = %s\n" (show $ d2d d6)
  printf "A.2) P dist of a D12 = %s\n" (show $ d2d d12)
  printf "B) P(sum = 11 | d6 && d12) = %.6f\n" (sumGT d6 d12 11)
  printf "C.1) P(d6 && d12 | bag) = %.6f\n"  (draw2 d6 d12 bag)
  printf "C.2) P(d20 && !d20 | bag) = %.6f\n" (drawOnce d20 bag)

  --print (pfilter (\a,b -> a+b) (show $ bind (d2d d6) (d2d d12))
  -- printf "B) P(sum = 11 | d6 && d12) = %.4f"

