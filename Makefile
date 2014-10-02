
all: dice

dice: dice.hs Infer.hs Dice.hs Tally.hs Gamble.hs
	ghc -O -XPackageImports --make dice.hs

clean:
	rm -f *.hi *.o dice

