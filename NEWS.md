# mRchmadness 1.0.4 - March 14, 2018

## Bug fixes
* fixed output of `add.home.bias()` to match `pred.pop.[league].[year]`
* fixed error on kenpom predictions due to first-round changes ([#22](https://github.com/elishayer/mRchmadness/issues/22))
* fixed lack of error checking of `bracket.picks` contents in `test.bracket` ([#24](https://github.com/elishayer/mRchmadness/issues/24))

## New features
* `draw.bracket()` leaves less white space

# mRchmadness 1.0.3 - March 13, 2018

## New data
* `bracket.men.2018` (update)
* `pred.538.men.2018` (update)
* `pred.kenpom.men.2018`
* `pred.pop.men.2018` (update)

## New features
* home team bias-adjustments for population picks
  (see `pool.bias` argument of `find.bracket` and `test.bracket()`)
* provide support for kenpom predictions

# mRchmadness 1.0.2 - March 12, 2018

## New data
* `bracket.women.2018`
* `pred.538.women.2018`

## Shiny updates
* App flipped to 2018
* App is men's only in this release, pending additional women's data

# mRchmadness 1.0.1 - March 11, 2018

## New data
* `bracket.men.2018`
* `bracket.women.2018` - from Charlie Creme's bracketology page on ESPN
* `games.men.2018`
* `games.women.2018`
* `pred.538.men.2018`
* `pred.pop.men.2018`

## New features
* handling of unplayed first-round games
