source('R/plot.bracket.R')

bracket = readRDS('data/bracket2017.rds')
games = readRDS('data/games2017.rds')

source('R/bradley.terry.R')
probability.matrix = bradley.terry(games)

source('R/simulate.bracket.R')

time = Sys.time()
candidates = simulate.bracket(bracket, probability.matrix, num.reps = 100000)
Sys.time() - time

time = Sys.time()
results = simulate.bracket(bracket, probability.matrix, num.reps = 10000)
Sys.time() - time

source('R/score.bracket.R')
## Takes 15 minutes
#time = Sys.time()
#scores = score.bracket(candidates, results)
#Sys.time() - time
