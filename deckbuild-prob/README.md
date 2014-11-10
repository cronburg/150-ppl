deckbuild-prob
==============

This is a workspace for modeling deck-building games in various probabilistic
programming languages. We are most familiar with the game *Dominion*, so we use
it as a starting point for designing the infrastructure required to specifically
model *Dominion*. We hope to abstract away the specifics of *Dominion* into an
all-encompassing framework for probabilistically modeling any deck-building game
given a specification for the entities and their interactions in the game.

BLOG and Church are the first language candidates that come to mind for modeling a
system with a probabilistic gaming state space. Both probabilistic events
(due to shuffling) and actions (due to player decisions and strategies) make
for an exponentially growing state space with the turn number. At the same time,
exact results are not required because we wish to model human behaviour in
these games, making the montecarlo sampling technique of BLOG and Church
ideal.

The game *Dominion* was created by *Donald X. Vaccarino*. Specific card content taken from
www.dominionstrategy.com.

