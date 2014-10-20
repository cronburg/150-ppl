
-- Pixel is necessarily black or white
data Pixel = BLACK | WHITE

-- Fraction of black pixels is proportional to probability that a bubble
-- is filled in:
data Bubble = P Pixel

-- INVARIANT: (length (support someRow)) == 36
data Row = P Bubble

-- INVARIANT: (length someGrid) == 8
data BubbleGrid = [Row]

type UTLN = String

scale     :: Positive Double -> Image -> P Image
translate :: Double -> Double -> Image -> P Image
rotate    :: Double -> Image -> P Image

-- Generates the probability distribution over BubbleGrids; given an expected
-- "perfect" image for some UTLN, and observed / scanned image.
singleUTLN :: Image -> Image -> P BubbleGrid
singleUTLN perfect scan = undefined

-- Generates the perfectly filled-in image that we expect the scanned image to look like
perfectImage :: Image -> UTLN -> Image
perfectImage base un = undefined

-- Generate a distribution over all UTLNs for the given scanned image
allUTLNs :: Image -> Image -> [UTLN] -> P UTLN
allUTLNs base scan uns =
  let dst1 :: P UTLN -- UTLN space
      dst1 = equally uns -- assume equal probability of all UTLNs for an arbitrary scan
      dst2 :: P (UTLN, BubbleGrid) -- (username, bubble grid) space
      dst2 = bindx dst1 (\un -> singleUTLN (perfectImage base un) scan)
      dst3 :: P UTLN -- map to only UTLN space
      dst3 = pmap fst dst2
  in dst3

-- Determine the most probable UTLN for the given scanned image
bestUTLN :: Image -> Image -> [UTLN] -> UTLN
bestUTLN baseImage scan uns = mostProb $ allUTLNs baseImage scans uns

