module Examples.UnitTests where

dBG = defaultBaseGame

test0 = defaultMoneyHeuristic $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER,COPPER]}
test1 = defaultMoneyHeuristic $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER]} } }
test1 = defaultMoneyHeuristic $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[COPPER]} } }
