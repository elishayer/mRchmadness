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
#'   "kenpom": Ken Pomeroy's predictions (kenpom.com), or
#'   "538": predictions form fivethirtyeight.com.
#'   Ignored if prob.matrix is specified.
#' @param pool.source source from which to use round probabilities to simulate
#'   entries of opponents in pool. Same options as prob.source.
#' @param league which league: "men" (default) or "women", for pool.source.
#' @param year year of tournament, used for prob.source.
#'   Ignored if prob.matrix is specified.
#' @param pool.bias character vector of names of teams to whom home-team bias
#'   is to be applied (must match name column of pred.pop.[league].[year]).
#'   Ignored unless pool.source is "pop" (see ?add.home.bias for details).
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
#' @param print.progress should progress be printed to console?
#' @param shiny.progress a shiny::Progress object used only for the Shiny app
#' @examples
#' prob.matrix = bradley.terry(games = games.men.2018)
#' my.bracket = find.bracket(bracket.empty = bracket.men.2018,
#'   prob.matrix = prob.matrix, pool.source = "pop", league = "men",
#'   year = 2018)
#' result = test.bracket(bracket.empty = bracket.men.2018,
#'   bracket.picks = my.bracket, prob.matrix = prob.matrix,
#'   pool.source = "pop", league = "men", year = 2018)
#' @export
#' @author sspowers
test.bracket22 = function(bracket.empty, bracket.picks, prob.matrix = NULL,
  prob.source = c(NULL,"pop", "kenpom", "538"),
  pool.source = c(NULL,"pop", "kenpom", "538"), league = c("men", "women"),
  year = 2018, pool.bias = NULL, pool.size = 30, num.sims = 1000,
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply"),
  print.progress = TRUE, shiny.progress = NULL) {

# Sanitize inputs
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }
  if (length(bracket.picks) != 63) {
    stop("Length of bracket.picks must be 63.")
  }
  if (!all(bracket.picks %in% bracket.empty)) {
    stop(paste0("All picked teams must be in the bracket (check on ",
      paste(bracket.picks[!bracket.picks %in% bracket.empty], collapse = ", "), ")"))
  }
  if (pool.size < 2) {
    stop("pool.size must be at least 2")
  }
  if("kenpom" %in% c(prob.source, pool.source) && (league == "women" || year < 2018)) {
    stop("kenpom is only available for the men's 2018 bracket.")
  }

# Update the user on progress
  if (print.progress) {
    cat('Testing your bracket ...')
    cat('\n  Simulating', num.sims, 'pools of size', pool.size, '...')
  }
  if (!is.null(shiny.progress)) {
    shiny.progress$set(detail =
      paste('Simulating', num.sims, 'pools of size', pool.size))
  }

# Simulate the rest of the pool for all simulations
  pool = sim.bracket22(bracket.empty = bracket.empty, prob.source = pool.source,
    league = league, year = year, home.teams = pool.bias,
    num.reps = num.sims * pool.size)
# Simulate the outcome for all simulations
  outcome = sim.bracket22(bracket.empty = bracket.empty,
    prob.matrix = prob.matrix.22, prob.source = prob.source, league = league,
    year = year, num.reps = num.sims)

# Set up the matrix to hold the scores of all brackets in pool
  score = matrix(NA, pool.size + 1, num.sims)

# Update the user on progress
  if (print.progress) {
    cat('\n  Scoring', num.sims * (1 + pool.size), 'brackets ... \n    ') 
  }
  if (!is.null(shiny.progress)) {
    shiny.progress$set(detail =
      paste('Scoring', num.sims * (1 + pool.size), 'brackets'))
  }
  
  for (i in 1:num.sims) {
# Extract brackets to be evaluated on this simulation
    brackets = cbind(bracket.picks, pool[, (i - 1) * pool.size + 1:pool.size])
# Score all of these brackets against outcome of this simulation
    score[, i] = score.bracket(bracket.empty = bracket.empty,
      bracket.picks = brackets, bracket.outcome = outcome[, i],
      bonus.round = bonus.round, bonus.seed = bonus.seed,
      bonus.combine = bonus.combine)
# Update the user on progress
    if (print.progress & (i %% 1000) == 0) {
      cat(paste0(round(100 * i / num.sims), '% '))
    }
    if (!is.null(shiny.progress)) shiny.progress$set(value = i)
  }
  if (print.progress) cat('\n')

# Get percentile finish for your bracket in each simulation
  rank = apply(score, 2, rank, ties.method = 'max')
  percentile = rank[1, ] / nrow(score)

# Check whether your bracket won in each simulation
  win = score[1, ] == apply(score, 2, max)

  list(score = score[1, ], percentile = percentile, win = win)
}
