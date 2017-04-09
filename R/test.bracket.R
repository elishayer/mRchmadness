#' Test a bracket
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param bracket.picks an length-63 character vector encoding your picks
#'   (this is the bracket to be evaluated)
#' @param prob.matrix a matrix of probabilities, with rows and columns
#'   corresponding to teams, matching the output of bradley.terry().
#'   This probabilities are used to simulate outcomes on
#'   which to evaluate bracket.picks. If NULL, prob.source is used.
#' @param prob.source source from which to use round probabilities to simulate
#'   outcomes --- "pop": ESPN's population of picks (default),
#'   "Pom": Ken Pomeroy's predictions (kenpom.com), or
#'   "538": predictions form fivethirtyeight.com.
#'   Ignored if prob.matrix is specified.
#' @param league which league: "men" (default) or "women", for pool.source.
#' @param year year of tournament, used for prob.source.
#'   Ignored if prob.matrix is specified.
#' @param pool.size number of brackets in your pool (excluding yours), matters
#'   only if criterion == "win" (default is 30)
#' @param num.sims number of simulations over which to evaluate the candidate
#'   brackets (default is 1000)
#' @param bonus.round a length-6 vector giving the number of points awarded in
#'   your pool's scoring rules for correct picks in each round (default is
#'   2^round)
#' @param bonus.seed a length-16 vector giving the bonus awarded for correctly
#'   picking winner based on winner's seed (default is zero)
#' @param bonus.combine how to combine the round bonus with the seed bonus to
#'   get the number of points awarded for each correct pick: "add" (default) or
#'   multiply
#' @examples
#' prob.matrix = bradley.terry(games = games.men.2017)
#' my.bracket = find.bracket(bracket.empty = bracket.men.2017,
#'   prob.matrix = prob.matrix, pool.source = "pop", league = "men",
#'   year = 2017)
#' result = test.bracket(bracket.empty = bracket.men.2017,
#'   bracket.picks = my.bracket, prob.matrix = prob.matrix,
#'   pool.source = "pop", league = "men", year = 2017)
#' @export
#' @author sspowers
test.bracket = function(bracket.empty, bracket.picks, prob.matrix = NULL,
  prob.source = c("pop", "Pom", "538"),
  pool.source = c("pop", "Pom", "538"), league = c("men", "women"),
  year = 2017, pool.size = 30, num.sims = 1000,
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

# Sanitize inputs
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }
  if (length(bracket.picks) != 63) {
    stop("Length of bracket.picks must be 63.")
  }
  if (pool.size < 2) {
    stop("pool.size must be at least 2")
  }

# Simulate the rest of the pool for all simulations
  pool = sim.bracket(bracket.empty = bracket.empty, prob.source = pool.source,
    league = league, year = year, num.reps = num.sims * pool.size)
# Simulate the outcome for all simulations
  outcome = sim.bracket(bracket.empty = bracket.empty,
    prob.matrix = prob.matrix, prob.source = prob.source, league = league,
    year = year, num.reps = num.sims)

# Set up the matrix to hold the scores of all brackets in pool
  score = matrix(NA, pool.size + 1, num.sims)
  
  for (i in 1:num.sims) {
# Extract brackets to be evaluated on this simulation
    brackets = cbind(bracket.picks, pool[, (i - 1) * pool.size + 1:pool.size])
# Score all of these brackets against outcome of this simulation
    score[, i] = score.bracket(bracket.empty = bracket.empty,
      bracket.picks = brackets, bracket.outcome = outcome[, i],
      bonus.round = bonus.round, bonus.seed = bonus.seed,
      bonus.combine = bonus.combine)
  }

# Get percentile finish for your bracket in each simulation
  rank = apply(score, 2, rank, ties.method = 'max')
  percentile = rank[1, ] / nrow(score)

# Check whether your bracket won in each simulation
  win = score[1, ] == apply(score, 2, max)

  list(score = score[1, ], percentile = percentile, win = win)
}
