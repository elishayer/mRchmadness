# # trying with all imported 2022 data...
# library(mRchmadnessCB)
# library(tidyverse)
games.men.2022 %>% head()


str(pred.pop.men.2022)
str(mRchmadness::pred.pop.men.2021)
mRchmadness::pred.pop.men.2016 %>% str()

write.csv(x = pred.pop.men.2021, file = "pred.pop.men.2021.csv")
read.csv(file = "pred.pop.men.2021.csv") %>% View()
set.seed(1)
prob.matrix.22 = mRchmadness::bradley.terry(games = games.men.2022)
str(prob.matrix.22)
prob.matrix.21 = mRchmadness::bradley.terry(games = games.men.2021)
library(clipr)
View(prob.matrix.22)
str(bracket.2022)
bracket.2022 %>% write_clip()
bracket.men.2022 %>% write_clip()
bracket.2022.test <- read_clip()
bracket.men.2022
r
set.seed(2017)
outcome = sim.bracket(bracket.empty = bracket.2022.test,
                      prob.matrix = prob.matrix.22)
draw.bracket(bracket.empty = bracket.2022.test, bracket.filled = outcome)
bracket.2022
pred.
str(bracket.2022.test)
str(bracket.men.2022)
set.seed(42)
my.bracket = find.bracket(bracket.empty = bracket.men.2021,
                            prob.matrix = prob.matrix.21, num.candidates = 100, num.sims = 1000,
                            criterion = "win", pool.size = 15, bonus.round = c(1, 2, 4, 8, 16, 32),
                            bonus.seed = rep(0, 16), bonus.combine = "add", year = 2021)
draw.bracket(bracket.empty = bracket.men.2021, bracket.filled = my.bracket)

set.seed(55)
my.bracket5000 = find.bracket22(bracket.empty = bracket.2022.final,
                            prob.matrix = prob.matrix.22,
                            num.candidates = 100000, num.sims = 5000,
                            criterion = "score", pool.size = 15, bonus.round = c(1, 2, 4, 8, 16, 32),
                            bonus.seed = rep(0, 16), bonus.combine = "add",
                            year = 2022,prob.source = NULL,pool.source = NULL)
draw.bracket(bracket.empty = bracket.2022.final, bracket.filled = my.bracket5000)
library(mRchmadness)
length(bracket.men.2021)
length(bracket.men.2022)


mRchmadness::pred.kenpom.men.2022
mRchmadness::pred.kenpom.men.2018

my.bracket <- find.bracket22(bracket.empty = bracket.2022.test,
                             prob.matrix = prob.matrix.22,year = 2022,
                             num.candidates = 100,num.sims = 1000,criterion = "win",
                             pool.size = 30,
                             bonus.round = c(1, 2, 4, 8, 16, 32),
                             bonus.seed = rep(0, 16), bonus.combine = "add",league = "men")


bracket.2022.test


dimnames(prob.matrix.22) %>% unique()
dim(prob.matrix.22)
dim(prob.matrix.21)

prob.matrix = prob.matrix.22
write_clip(my.bracket)
set.seed(8675309)
test = test.bracket22(bracket.empty = bracket.2022.test,
                    bracket.picks = read_clip(), prob.matrix = prob.matrix.22, pool.size = 15,
                    num.sims = 10000, bonus.round = c(1, 2, 4, 8, 16, 32),
                    bonus.seed = rep(0, 16), bonus.combine = "add",year = 2022)
hist(test$score, breaks = 20)
hist(test$percentile, breaks = 20)
mean(test$win)

bracket.2022.final <- read_clip()

bracket.2022.test
bracket.men.2022
write_clip(bracket.men.2022)
write_clip(bracket.2022.final)
