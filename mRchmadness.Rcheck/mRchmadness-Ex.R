pkgname <- "mRchmadness"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('mRchmadness')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("scrape.population.distribution")
### * scrape.population.distribution

flush(stderr()); flush(stdout())

### Name: scrape.population.distribution
### Title: Scrape the average rate of teams being picked to win across all
###   ESPN brackets
### Aliases: scrape.population.distribution

### ** Examples

populationDistribution = scrape.population.distribution(2017)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
