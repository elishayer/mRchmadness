#' Fill out a bracket based on some criteria
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param probability.matrix a matrix of probabilities, with rows and columns
#'   corresponding to teams, matching the output of bradley.terry()
#' @param pool.size number of brackets in your pool, matters only if
#'   criterion == "win" (default is 30)
#' @param num.candidates number of random brackets to try, taking the best one
#'   (default is 100)
#' @param num.sims number of simulations over which to evaluate the candidate
#'   brackets (default is 1000)
#' @param criterion how to choose among candidate brackets:
#'   "percentile" (default, maximize expected percentile within pool),
#'   "score" (maximize expected number of points) or "win" (maximize probabilty
#'   of winning pool).
#' @param bonus.round a length-6 vector giving the number of points awarded in
#'   your pool's scoring rules for correct picks in each round (default is
#'   2^round)
#' @param bonus.seed a length-16 vector giving the bonus awarded for correctly
#'   picking winner based on winner's seed (default is zero)
#' @param bonus.combine how to combine the round bonus with the seed bonus to
#'   get the number of points awarded for each correct pick: "add" (default) or
#'   multiply
#' @returns the length-63 character vector describing the filled bracket which
#'   performs best according to criterion among all num.candidates brackets
#'   tried, across num.sims simulations of a pool of pool.size with scoring
#'   rules specified by bonus.round, bonus.seed and bonus.combine
#' @author sspowers
find.bracket = function(bracket.empty, probability.matrix, pool.size = 30,
  num.candidates = 100, num.sims = 1000,
  criterion = c("percentile", "score", "win"),
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

  criterion = match.arg(criterion)

# Input sanitization
  if (!is.numeric(bonus.round) | length(bonus.round) != 6) {
    stop("bonus.round must be length-6 numeric vector")
  }
  if (!is.numeric(bonus.seed) | length(bonus.seed) != 16) {
    stop("bonus.seed must be length-16 numeric vector")
  }

# Simulate the brackets to be considered
  candidates = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.candidates)

# Simulate all of the pools (across all simulations)
  pool = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims * pool.size)

# Simulate all of the outcomes
  outcome = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims)

# Prepare matrix to store all bracket scores
  score = matrix(NA, pool.size + num.candidates, num.sims)
  
  for (i in 1:num.sims) {
# Extract brackets to be evaluated on this simulation
    brackets = cbind(pool[, (i - 1) * pool.size + 1:pool.size], candidates)
# Score all of these brackets against outcome of this simulation
    score[, i] = score.bracket(bracket.empty = bracket.empty, 
      bracket.picks = brackets, bracket.outcome = outcome[, i],
      bonus.round = bonus.round, bonus.seed = bonus.seed,
      bonus.combine = bonus.combine)
  }

# Find bracket with highest average percentile finish (compare only to pool)
  if (criterion == "percentile") {
    rank = apply(score[-(1:pool.size), ], 2, rank, ties.method = 'max')
    percentile = rank / nrow(score)
    return(candidates[, which.max(rowMeans(percentile))])
  }

# Find bracket with highest average score
  if (criterion == "score") {
    return(candidates[, which.max(rowMeans(score[-(1:pool.size), ]))])
  }

# Find bracket which wins most
  if (criterion == "win") {
    win = score[-(1:pool.size), ] >= apply(score[1:pool.size, ], 2, max)
    return(candidates[, which.max(rowMeans(win))])
  }
}  
