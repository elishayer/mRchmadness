#' Simulate the full bracket starting with an empty bracket
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param probability.matrix a matrix of probabilities, with rows and columns
#'   corresponding to teams, matching the output of bradley.terry()
#' @param num.reps number of simulations to perform (default is 1)
#' @return a 63-by-num.reps matrix storing the simulation outcome, each
#'   column encoding the outcome for a single simulation in the following
#'   order: seeds 1 through 32 after round 1, seeds 1 through 16 after round 2,
#'   seeds 1 through 8 after round 3, seeds 1 through 4 after round 4,
#'   seeds 1 and 2 after round 5, and finally seed 1 after round 6 (the
#'   champion)
#' @examples
#' probability.matrix = bradley.terry(games.2017)
#' sim.bracket(bracket.2017, probability.matrix)
#' @export
#' @author sspowers
sim.bracket = function(bracket.empty, probability.matrix, num.reps = 1) {

  `%>%` = dplyr::`%>%`

# Sanitize inputs
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }

# Prepare matrix in which to store the outcomes of the simulation
  outcome = matrix('', 63, num.reps)

# Get the round number correspond to each row of outcome
  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)

# Use the fold function to arrange the teams in "matchup order"
  teams = bracket.empty %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16) %>% fold(32) %>% rep(num.reps)

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

# Loop over rounds (simulate round for all simulations simultaneously)
  for (r in 1:6) {
# Represent games as 2-column matrix with one row for each game
    matchups = matrix(teams, nrow = 2^(6 - r) * num.reps, ncol = 2,
      byrow = TRUE)
# Randomly select one team from each row of matchup matrix
    teams = matchups[1:nrow(matchups) + nrow(matchups) *
      (1 - stats::rbinom(nrow(matchups), 1, probability.matrix[matchups]))]
# Store outcomes from this round (across all simulations) in corresponding rows
    outcome[round == r, ] = matrix(teams, nrow = 2^(6 - r),
      ncol = num.reps)[untangling.indices[[r]], ]
  }
  outcome
}
