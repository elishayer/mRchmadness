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
sim.bracket.by.round = function(bracket.empty, probability.matrix,
  num.reps = 1, source = c("population", "538", "KenPom")) {

  `%>%` = dplyr::`%>%`

  source = match.arg(source)

# Sanitize inputs
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }

  if (source == "population") {
    mRchmadness::

# Prepare matrix in which to store the outcomes of the simulation
  outcome = matrix('', 63, num.reps)

# Get the round number correspond to each row of outcome
  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)

# Use the fold function to arrange the teams in "matchup order"
  teams = bracket.empty %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16) %>% fold(32) %>% rep(num.reps)





  probs = cbind(t(apply(-pred.538.2017[teams, 2:7], 1, diff)),
    pred.538.2017[teams, "round6"])
  colnames(probs) = paste0("round", 1:6)
  probs[is.na(rowSums(probs))] = 0.000000001

  path = matrix(0, 64, 6)

  for (r in 1:6) {
    path[, r] = max(path) + rep(seq(2^(6-r)), each = 2^r)
    groups = split(1:64, f = path[, r])
    outcome[round == r, ] = t(sapply(groups,
      function(i) {sample(i, num.reps, prob = probs[i, 6], replace = TRUE)}))
  }

  for (r in 2:6) {
    winners = outcome[round == r, , drop = FALSE]
    rows = path[as.numeric(winners), 1:(r-1), drop = FALSE]
    columns = matrix(rep(1:num.reps, each = nrow(winners)),
      nrow = length(winners), ncol = r - 1)
    outcome[cbind(c(rows), c(columns))] = rep(winners, times = ncol(rows))
  }

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

  for (r in 1:5) {
    outcome[round == r, ] = outcome[round == r, ][untangling.indices[[r]], ]
  }

  outcome
}
