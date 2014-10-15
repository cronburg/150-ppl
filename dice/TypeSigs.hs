rTF        :: (Real a, Fractional b) => a -> b
fI         :: (Integral a, Num b)    => a -> b

bindx      :: (Eq a, Eq b) => P a -> (a -> P b) -> P (a,b)

count      :: Eq a => a -> [a] -> Int 
equally    :: Eq a => [a] -> P a 
probOf     :: Eq a => a -> P a -> Double
liftPn     :: Eq a => [P a] -> P [a] 
liftPnBag2 :: Eq a => [P a] -> P (Bag a)
liftPnBag  :: Eq a => [P a] -> P [a] 
pmap       :: Eq b => (a -> b) -> P a -> P b 
regroup    :: Eq a => P a -> P a 
mostProb   :: Eq a => Int -> P a -> (a,Double)

expected :: Real a => P a -> Double

xor       :: Bool -> Bool -> Bool
support   :: P a -> [a] 
liftP     :: P a -> P b -> P (a,b)
normalize :: P a -> P a 
weight    :: P a -> Double
isnorm    :: P a -> Bool
pfilter   :: (a -> Bool) -> P a -> P a 
dfilter   :: (a -> Bool) -> P a -> P a
