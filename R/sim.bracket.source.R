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
#' @author sspowers
sim.bracket.source = function(prob.source, year, num.reps,
  outcome, round, teams, untangling.indices) {

  `%>%` = dplyr::`%>%`

# Load round probabilities from source and merge with teams dataframe to get id
  prob = parse(text =
      paste("mRchmadness::pred", prob.source, year, sep = ".")) %>%
    eval() %>% dplyr::left_join(mRchmadness::teams,
      by = c("name" = paste0("name.", prob.source)))
  rownames(prob) = prob$id

# Check that we have predictions for all teams in bracket
  missing.teams = setdiff(teams, prob$id)
  if (length(missing.teams) > 0) {
    print(mRchmadness::teams %>% dplyr::filter(id %in% missing.teams))
    stop("No predictions from source for above teams. Is year correct?")
  }

# Derived probability in round r is equal to probability of winning in round r,
# minus probability of winning in round r+1. This is the probability of winning
# in round r and losing in round r+1, which is how we draw winners of each
# round.
  prob.derived = -prob[teams, grep("round", colnames(prob))] %>%
    apply(MARGIN = 1, FUN = diff) %>% t() %>% cbind(prob[teams, "round6"])
  rownames(prob.derived) = colnames(prob.derived) = NULL

# Set up the variable to store the path (in terms of game #) that each team
# would need to take to get to the championship. This will be used later to
# ensure that teams who win in round r also win in rounds r-1, ..., 1.
  path = matrix(0, 64, 6)

# Simulate winners of each round
  for (r in 1:6) {
    path[, r] = max(path) + rep(seq(2^(6-r)), each = 2^r)
# Split teams into groups eligible for each slot in the bracket
    groups = split(1:64, f = path[, r])
# Choose among those eligible teams within each group based on derived probs
    outcome[round == r, ] = t(sapply(groups, function(i) {
      sample(i, num.reps, prob = prob.derived[i, 6], replace = TRUE)}))
  }

# Ensure that teams who win in round r also win in rounds r-1, ..., 1
  for (r in 2:6) {
# Identify winners of round r
    winners = outcome[round == r, , drop = FALSE]
# Find all games those winners must of won in rounds r-1, ..., 1
    rows = path[as.numeric(winners), 1:(r-1), drop = FALSE]
# Find which simulation to which each of those games corresponds
    columns = matrix(rep(1:num.reps, each = nrow(winners)),
      nrow = length(winners), ncol = r - 1)
# Replace the corresponding winners with the implied winners
    outcome[cbind(c(rows), c(columns))] = rep(winners, times = ncol(rows))
  }

# Re-order teams from matchup order back to seed order
  for (r in 1:5) {
    outcome[round == r, ] = outcome[round == r, ][untangling.indices[[r]], ]
  }

  outcome
}
