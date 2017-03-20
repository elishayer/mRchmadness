`%>%` = dplyr::`%>%`

source('R/fold.R')
source('R/unfold.R')

empty = readRDS('data/bracket2017.rds')
games = readRDS('data/games2017-old.rds')

source('R/scrape.population.distribution.R')
pop.picks2017 = scrape.population.distribution(2017)

source('R/bradley.terry.R')
probability.matrix = bradley.terry(games)

source('R/simulate.bracket.R')
picks = simulate.bracket(bracket.empty, probability.matrix, 30)
outcome = simulate.bracket(bracket.empty, probability.matrix)

source('R/score.bracket.R')
score.bracket(empty, picks, outcome)

source('R/find.bracket.R')
my.bracket = find.bracket(bracket.empty, probability.matrix)

source('R/plot.bracket.R')
plot.bracket(bracket.empty, my.bracket)

source('R/test.bracket.R')
test = test.bracket(bracket.empty, probability.matrix, my.bracket)


