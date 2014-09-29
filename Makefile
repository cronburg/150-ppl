
all: dice

dice: dice.hs Infer.hs
	ghc -O -XPackageImports --make dice.hs

clean:
	rm -f *.hi *.o dice

