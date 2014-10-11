
all: dice pfilter

dice: dice.hs Infer.hs Dice.hs Tally.hs Gamble.hs
	ghc -O -XPackageImports --make dice.hs

pfilter: pfilter.hs Infer.hs Dice.hs Tally.hs Gamble.hs
	ghc -O -XPackageImports --make pfilter.hs

clean:
	rm -f *.hi *.o dice pfilter

