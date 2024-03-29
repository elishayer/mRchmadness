#' Simulate the full bracket starting with an empty bracket
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param prob.matrix a matrix of probabilities, with rows and columns
#'   corresponding to teams, matching the output of bradley.terry().
#'   If NULL, prob.source is used.
#' @param prob.source source from which to use round probabilities for
#'   simulation --- "pop": ESPN's population of picks (default),
#'   "kenpom": Ken Pomeroy's predictions (kenpom.com), or
#'   "538": predictions form fivethirtyeight.com.
#'   Ignored if prob.matrix is specified.
#' @param league which league: "men" (default) or "women", for prob.source.
#'   Ignored if prob.matrix is specified.
#' @param year year of tournament, used for prob.source.
#'   Ignored if prob.matrix is specified.
#' @param home.teams character vector of names of teams to whom home-team bias
#'   is to be applied (must match name column of pred.pop.[league].[year]).
#'   Ignored unless prob.source is "pop" (see ?add.home.bias for details).
#' @param num.reps number of simulations to perform (default is 1)
#' @return a 63-by-num.reps matrix storing the simulation outcome, each
#'   column encoding the outcome for a single simulation in the following
#'   order: seeds 1 through 32 after round 1, seeds 1 through 16 after round 2,
#'   seeds 1 through 8 after round 3, seeds 1 through 4 after round 4,
#'   seeds 1 and 2 after round 5, and finally seed 1 after round 6 (the
#'   champion)
#' @examples
#' sim.bracket(bracket.empty = bracket.men.2018, prob.source = "538",
#'   league = "men", year = 2018)
#' @export
#' @author sspowers
sim.bracket = function(bracket.empty, prob.matrix = NULL,
  prob.source = c("pop", "kenpom", "538"), league = c("men", "women"),
  year = current.year, home.teams = NULL, num.reps = 1) {

  `%>%` = dplyr::`%>%`

# Sanitize inputs
  prob.source = match.arg(prob.source)
  league = match.arg(league)
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }

# Prepare matrix in which to store the outcomes of the simulation
  outcome = matrix('', 63, num.reps)

# Get the round number correspond to each row of outcome
  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)

# Use the fold function to arrange the teams in "matchup order"
  teams.remaining = bracket.empty %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16) %>% fold(32)

# The following chunk of code is a currently necessary evil.
# To simulate quickly, teams need to be in matchup order, but we want to
# store the outcome in seed order within each round.
# The untangling indices take care of this.
  untangling.indices = list()
  untangling.indices[[1]] = 1:32 %>% unfold(16) %>% unfold(8) %>% unfold(4) %>%
    unfold(2) %>% unfold(1)
  untangling.indices[[2]] = 1:16 %>% unfold(8) %>% unfold(4) %>% unfold(2) %>%
    unfold(1)
  untangling.indices[[3]] = 1:8 %>% unfold(4) %>% unfold(2) %>% unfold(1)
  untangling.indices[[4]] = 1:4 %>% unfold(2) %>% unfold(1)
  untangling.indices[[5]] = 1:2 %>% unfold(1)
  untangling.indices[[6]] = 1

  if (!is.null(prob.matrix)) {
    outcome = sim.bracket.matrix(prob.matrix = prob.matrix, league = league,
      num.reps = num.reps, outcome = outcome, round = round,
      teams.remaining = teams.remaining,
      untangling.indices = untangling.indices)
  } else {
    outcome = sim.bracket.source(prob.source = prob.source, league = league,
      year = year, home.teams = home.teams, num.reps = num.reps,
      outcome = outcome, round = round, teams.remaining = teams.remaining,
      untangling.indices = untangling.indices)
  }
  outcome
}

