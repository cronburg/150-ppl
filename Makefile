
all: hello dice tally gamble

hello: hello.hs
	ghc -o hello hello.hs

dice: dice.hs Infer.hs
	ghc -XPackageImports --make dice.hs

tally: tally.hs Infer.hs
	ghc -XPackageImports --make tally.hs

gamble: gamble.hs Infer.hs
	ghc -XPackageImports --make gamble.hs

