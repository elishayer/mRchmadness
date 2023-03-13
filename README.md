# mRchmadness
mRchmadness is *not* designed to predict the winners of tournament games, but
it *is* designed to answer the question: *Given the true probabilities
dictating tournament results and given the probabilities with which my pool
opponents make their picks, what bracket maximizes my chances of winning my
pool?* For example, do you want to pick the team with the highest probability
of winning if almost everybody else in your pool is making the same pick?
To answer these questions, we simulate tournament results and opponent
picks over and over again, testing a set of candidate brackets against pool
opponents in each simulation and choosing the bracket that performs best.

## Installation

``` r
# For the latest version of the package, updated frequently during the first
# couple weeks of March, install directly from this GitHub repository.
devtools::install_github('elishayer/mRchmadness')
```

## Introduction

``` r
# For a tutorial on the primary use case of the package, check out the vignette
vignette('mRchmadness')
```

## Shiny

For those who prefer a point-and-click interface, check out our
[Shiny app](https://saberpowers.shinyapps.io/mRchmadness/).

You can also use the Shiny app locally by running
`shiny::runApp('inst/shinyApp')` from the mRchmadness directory.
