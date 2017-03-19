library(dplyr)

source('R/fold.R')
source('R/unfold.R')

bracket = readRDS('data/bracket2017.rds')
games = readRDS('data/games2017-old.rds')

source('R/bradley.terry.R')
probability.matrix = bradley.terry(games)

source('R/simulate.bracket.R')

source('R/score.bracket.R')
## Takes 15 minutes
#time = Sys.time()
#scores = score.bracket(candidates, results)
#Sys.time() - time

source('R/find.bracket.R')
my.bracket = find.bracket(bracket, probability.matrix)

source('R/plot.bracket.R')
plot.bracket(bracket, my.bracket)

source('R/test.bracket.R')
test = test.bracket(bracket, probability.matrix, my.bracket)


