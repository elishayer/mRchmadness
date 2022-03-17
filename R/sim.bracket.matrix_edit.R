#' Simulate the full bracket starting with an empty bracket
#'
#' @param prob.matrix a matrix of probabilities, with rows and columns
#'   corresponding to teams, matching the output of bradley.terry()
#' @param league which league: "men" (default) or "women"
#' @param num.reps number of simulations to perform
#' @param outcome passed in from sim.bracket()
#' @param round passed in from sim.bracket()
#' @param teams.remaining passed in from sim.bracket()
#' @param untangling.indices passed in from sim.bracket()
#' @return a 63-by-num.reps matrix storing the simulation outcome, each
#'   column encoding the outcome for a single simulation in the following
#'   order: seeds 1 through 32 after round 1, seeds 1 through 16 after round 2,
#'   seeds 1 through 8 after round 3, seeds 1 through 4 after round 4,
#'   seeds 1 and 2 after round 5, and finally seed 1 after round 6 (the
#'   champion)
#' @author sspowers
sim.bracket.matrix22 = function(prob.matrix, league, num.reps, outcome, round,
  teams.remaining, untangling.indices) {

# Load the dataframe of teams corresponding to the correct league
  teams = eval(parse(text = paste("mRchmadness::teams", league, sep = ".")))

# Handle the case where some first-round games have not yet been decided by
# creating a composite team whose win/loss probabilities are the weighted
# averages of the two competing teams, weighted by how likely each team wins
  # Identify teams in first-round games and advancement probabilities
  teams.tbd = teams.remaining[grep('/', teams.remaining)]
  if (length(teams.tbd) > 0) {
    teams.tbd.split = t(sapply(strsplit(teams.tbd, '/'), identity))
    probs.tbd = prob.matrix[teams.tbd.split]
    # Add new rows to prob.matrix corresponding to composite of teams TBD
    new.rows = probs.tbd * prob.matrix[teams.tbd.split[, 1], ] +
      (1 - probs.tbd) * prob.matrix[teams.tbd.split[, 2], ]
    rownames(new.rows) = teams.tbd
    prob.matrix = rbind(prob.matrix, new.rows)
    # Add new columns to prob.matrix corresponding to composite of teams TBD
    new.cols = t(probs.tbd * t(prob.matrix[, teams.tbd.split[, 1]]) +
      (1 - probs.tbd) * t(prob.matrix[, teams.tbd.split[, 2]]))
    colnames(new.cols) = teams.tbd
    prob.matrix = cbind(prob.matrix, new.cols)
  }

# Check that we have rows and columns in prob.matrix for all teams in bracket
  missing.rows = setdiff(teams.remaining, rownames(prob.matrix.22))
  if (length(missing.rows) > 0) {
    print(teams[teams$id %in% missing.rows, ])
    stop("prob.matrix has no rows for above teams. Row names must be team ID.")
  }
  missing.cols = setdiff(teams.remaining, colnames(prob.matrix.22))
  if (length(missing.cols) > 0) {
    print(teams[teams$id %in% missing.cols, ])
    stop("prob.matrix has no cols for above teams. Col names must be team ID.")
  }

# Put the brackets for all simulations together for simultaneous simulation
  teams.remaining = rep(teams.remaining, times = num.reps)

# Loop over rounds (simulate round for all simulations simultaneously)
  for (r in 1:6) {
# Represent games as 2-column matrix with one row for each game
    matchups = matrix(teams.remaining, nrow = 2^(6 - r) * num.reps, ncol = 2,
      byrow = TRUE)
# Randomly select one team from each row of matchup matrix
    teams.remaining = matchups[1:nrow(matchups) + nrow(matchups) *
      (1 - stats::rbinom(nrow(matchups), 1, prob.matrix.22[matchups]))]
# Store outcomes from this round (across all simulations) in corresponding rows
    outcome[round == r, ] = matrix(teams.remaining, nrow = 2^(6 - r),
      ncol = num.reps)[untangling.indices[[r]], ]
  }
  outcome
}
