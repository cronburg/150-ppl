
all: hello dice tally gamble

hello: hello.hs
	ghc -O -o hello hello.hs

dice: dice.hs Infer.hs
	ghc -O -XPackageImports --make dice.hs

tally: tally.hs Infer.hs
	ghc -O -XPackageImports --make tally.hs

gamble: gamble.hs Infer.hs
	ghc -O -XPackageImports --make gamble.hs

clean: hello dice tally gamble
	rm -f *.hi *.o hello dice tally gamble

