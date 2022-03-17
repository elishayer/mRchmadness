# adding data for 2022
library(tidyverse)
getwd()

# import csv files
pred.kenpom.men.2022 <- read.csv("pred.kenpom.men.2022.csv",as.is = F)
pred.538.men.2022 <- read.csv("pred.538.men.2022.csv", as.is = F,)
pred.pop.men.2022 <- read.csv("pred.pop.men.2022.csv",as.is = F)

# write .Rdata files
save(pred.kenpom.men.2022,file = "C:/Users/cbran/OneDrive/Documents/Birkie Music/2022 Birkie/NCAA_2022/data/pred.kenpom.men.2022.RData")
save(pred.538.men.2022, file = "C:/Users/cbran/OneDrive/Documents/Birkie Music/2022 Birkie/NCAA_2022/data/pred.538.men.2022.Rdata")
save(pred.pop.men.2022, file = "C:/Users/cbran/OneDrive/Documents/Birkie Music/2022 Birkie/NCAA_2022/data/pred.pop.men.2022.RData")
library(clipr)
# write file in R folder
# also did this, just by saving as a new file

# check .Rbuildignore file
bracket.2022 <- read_clip()
save(bracket.2022,file = "./data/bracket.2022.Rdata")
teams_2022 <- teams.men %>% filter(id %in% bracket.2022)
View(teams_2022)
write_clip(teams_2022$name,object_type = "character")
bracket.men.2022

bracket.2022.test %>% write_clip()

library(tidyverse)
