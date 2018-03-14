#' Add the effect of home-team bias to population picks
#'
#' @param teams character vector of names of teams to whom home-team bias is to
#'   be applied (must match name column of pred.pop.[league].[year])
#' @param league which league: "men" (default) or "women", for prob.source.
#' @param year year of tournament
#' @return a 64-by-6 matrix matching population pick matrix from year, except
#'   that specified teams benefit from greater advancement probability
#'   attributable to home-team bias. Rows correspond to teams, and columns
#'   correspond to rounds, with the values giving the probability of each team
#'   winning in each round.
#' @details This function implements a rule of thumb gleamed from an article
#'   published at CBS Sports on March 10, 2016, by Brad Null of Bracket Voodoo,
#'   titled "Homer bias is real and it will derail your March Madness bracket".
#'   The sparse-detail results published there are consistent with adding a
#'   constant +3/4 to the log-odds of the conditional probability of the "home"
#'   team winning in each round. For more details, see
#'   https://github.com/elishayer/mRchmadness/issues/13. Null cautions in his
#'   piece that "schools with smaller undergraduate enrollment (Duke, Xavier,
#'   Villanova) tend to have a smaller regional footprint", so only use this
#'   adjustment if you have a good reason to think the picks in your pool would
#'   be biased (for example a pool of UNC alumni).
#' @examples
#' add.home.bias(teams = 'UNC', league = 'men', year = 2018)
#' @export
#' @author sspowers
add.home.bias = function(teams, league = c("men", "women"), year = 2018) {

  `%>%` = dplyr::`%>%`

# Sanitize inputs
  league = match.arg(league)

# Load round probabilities from source
  prob = eval(parse(text =
      paste("mRchmadness::pred.pop", league, year, sep = ".")))

# Check that teams match name column of prob
  missing.teams = setdiff(teams, prob$name)
  if (length(missing.teams > 0)) {
    print(missing.teams)
    stop(paste0("The above teams were not found in pred.pop.",
      league, year, sep = "."))
  }

# Get probability of winning each round, conditional on winning previous round
  prob.conditional = as.matrix(prob[, -1])
  rownames(prob.conditional) = prob$name
  prob.conditional[, 2] = prob$round2 / prob$round1
  prob.conditional[, 3] = prob$round3 / prob$round2
  prob.conditional[, 4] = prob$round4 / prob$round3
  prob.conditional[, 5] = prob$round5 / prob$round4
  prob.conditional[, 6] = prob$round6 / prob$round5

# Convert from probabilities to log-odds
  odds.conditional = log(prob.conditional / (1 - prob.conditional))
# Apply home-team bias adjustment by adding 3/4 to conditional log odds
  odds.conditional.bias = odds.conditional
  odds.conditional.bias[teams, ] = odds.conditional.bias[teams, ] + 3/4
# Convert back from log-odds to probabilities
  prob.conditional.bias =
    exp(odds.conditional.bias) / (1 + exp(odds.conditional.bias))
# Probabilities that are 0 or 1 will become NaN after being converted to
# log-odds and back again, so the line below restores them
  prob.conditional.bias[!is.finite(prob.conditional.bias)] =
    prob.conditional[!is.finite(prob.conditional.bias)]

# Convert conditional probabilities back into round advancement probabilities
  prob.bias = prob.conditional.bias
  prob.bias[, 2] = prob.bias[, 1] * prob.conditional.bias[, 2]
  prob.bias[, 3] = prob.bias[, 2] * prob.conditional.bias[, 3]
  prob.bias[, 4] = prob.bias[, 3] * prob.conditional.bias[, 4]
  prob.bias[, 5] = prob.bias[, 4] * prob.conditional.bias[, 5]
  prob.bias[, 6] = prob.bias[, 5] * prob.conditional.bias[, 6]

# Re-normalize columns of round advancement probability matrix so that the
# expected number of teams advancing to each round is correct
  prob.bias = t(t(prob.bias) / colSums(prob.bias) * 2^(5:0))
  prob[, -1] = prob.bias
  prob
}

