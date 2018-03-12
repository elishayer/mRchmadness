#' Simulate the full bracket starting with an empty bracket
#'
#' @param prob.source source from which to use round probabilities for
#'   simulation --- "pop": ESPN's population of picks,
#'   "Pom": Ken Pomeroy's predictions (kenpom.com), or
#'   "538": predictions form fivethirtyeight.com.
#' @param league which league: "men" (default) or "women", for prob.source.
#' @param year year of tournament, used for prob.source.
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
sim.bracket.source = function(prob.source, league, year, num.reps, outcome,
  round, teams.remaining, untangling.indices) {

  `%>%` = dplyr::`%>%`

# Load round probabilities from source
  prob = eval(parse(text =
      paste("mRchmadness::pred", prob.source, league, year, sep = ".")))
# Load the dataframe of teams corresponding to the correct league
  teams = eval(parse(text = paste("mRchmadness::teams", league, sep = ".")))

# Get team ids to match tournament teams to probabilities from source
  team.id = as.character(teams$id)
  names(team.id) = teams[, paste0("name.", prob.source)]

# Look up team IDs to use as row names
  rownames(prob) = prob$name %>%
# Handle unplayed first-round games signified by "/"
    as.character %>% strsplit(split = '/') %>%
    lapply(function(name) as.character(team.id[name])) %>%
    sapply(function(ids) do.call(paste, args = as.list(c(ids, sep = '/'))))
# Drop team names from prob
  prob$name = NULL

# Check that we have predictions for all teams in bracket
  missing.teams = setdiff(teams.remaining, rownames(prob))
  if (length(missing.teams) > 0) {
    print(missing.teams)
    stop("No predictions from source for above teams. Is year correct?")
  }

# Derived probability in round r is equal to probability of winning in round r,
# minus probability of winning in round r+1. This is the probability of winning
# in round r and losing in round r+1, which is how we draw winners of each
# round.
  prob.derived = -prob[, grep("round", colnames(prob))] %>%
    apply(MARGIN = 1, FUN = diff) %>% t() %>% cbind(prob$round6)
  colnames(prob.derived) = paste0("round", 1:6)
  prob.derived = prob.derived[teams.remaining, ]

# Set up the variable to store the path (in terms of game #) that each team
# would need to take to get to the championship. This will be used later to
# ensure that teams who win in round r also win in rounds r-1, ..., 1.
  path = matrix(0, 64, 6)
  rownames(path) = teams.remaining

# Simulate winners of each round
  for (r in 1:6) {
    path[, r] = max(path) + rep(seq(2^(6-r)), each = 2^r)
# Split teams into groups eligible for each slot in the bracket
    groups = split(teams.remaining, f = path[, r])
# Choose among those eligible teams within each group based on derived probs
    outcome[round == r, ] = t(sapply(groups, function(i) {
      sample(i, num.reps, prob = prob.derived[i, r], replace = TRUE)}))
  }

# Ensure that teams who win in round r also win in rounds r-1, ..., 1
  for (r in 2:6) {
# Identify winners of round r
    winners = outcome[round == r, , drop = FALSE]
# Find all games those winners must have won in rounds r-1, ..., 1
    rows = path[winners, 1:(r-1), drop = FALSE]
# Find which simulation to which each of those games corresponds
    columns = matrix(rep(1:num.reps, each = nrow(winners)),
      nrow = length(winners), ncol = r - 1)
# Replace the corresponding winners with the implied winners
    outcome[cbind(c(rows), c(columns))] = rep(winners, times = ncol(rows))
  }

# Re-order teams from matchup order back to seed order
  for (r in 1:5) {
    outcome[round == r, ] =
      outcome[round == r, , drop = FALSE][untangling.indices[[r]], ]
  }

  outcome
}
