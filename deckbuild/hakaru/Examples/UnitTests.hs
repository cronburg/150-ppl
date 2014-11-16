{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-} 
module Examples.UnitTests where
import System.IO.Unsafe (unsafePerformIO)
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Base
import Examples.Base
import Control.Monad.State
import Test.HUnit hiding (test)
-- TODO: setup modules such that they export other modules required to use them
--       (e.g. Control.Monad.State in Lib)

-- Regression tests:
test = runTestTT tests
tests = TestList
  [ TestLabel "dMH" dmh0_test
  , TestLabel "dMH" dmh0_test
  , TestLabel "dMH" dmh0_test
  ]
mkTest   s e r = TestCase $ assertEqual s e r
mkTestIO s e r = TestCase $ assertEqual s e (unsafePerformIO r)

-- Macros
dBG = defaultBaseGame
dMH = defaultMoneyHeuristic

dmh0_result  = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER,COPPER]} } }
dmh0_expects = Just SILVER
dmh0_test   = mkTestIO "dMH" dmh0_expects dmh0_result

--dmh_test1 = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER]} } }
--dmh_test1 = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[COPPER]} } }


