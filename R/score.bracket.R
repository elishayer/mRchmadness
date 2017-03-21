#' Compute score for bracket given actual result
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param bracket.picks an length-63 character vector encoding the picks
#'   (this is the bracket to be evaluated)
#' @param bracket.outcome a 63-row matrix encoding the outcome of multiple
#'   simulations of the tournament. bracket.picks will be scored against each
#'   outcome
#' @param bonus.round a length-6 vector giving the number of points awarded in
#'   your pool's scoring rules for correct picks in each round (default is
#'   2^round)
#' @param bonus.seed a length-16 vector giving the bonus awarded for correctly
#'   picking winner based on winner's seed (default is zero)
#' @param bonus.combine how to combine the round bonus with the seed bonus to
#'   get the number of points awarded for each correct pick: "add" (default) or
#'   multiply
#' @return a vector giving the score for bracket.picks for each outcome in
#'   the matrix bracket.oucome
#' @author sspowers
score.bracket = function(bracket.empty, bracket.picks, bracket.outcome,
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

  bonus.combine = match.arg(bonus.combine)

# Sanitize inputs
  if (length(bonus.round) != 6) {
    stop("Length of bonus.round must be 6.")
  }
  if (length(bonus.seed) != 16) {
    stop("Length of bonus.seed must be 16.")
  }

# Get round bonus for each row of bracket.outcome
  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)
  round.bonus = bonus.round[round]

# Get seed bonus for each entry of bracket.outcome
  seed = rep(1:16, each = 4)
  names(seed) = bracket.empty
  seed.bonus = matrix(bonus.seed[seed[bracket.picks]], 63, ncol(bracket.picks))

# For each entry in bracket.outcome, get # of points for correctly picking it
  points = switch(bonus.combine,
    add = round.bonus + seed.bonus,
    multiply = round.bonus * seed.bonus)

# Add up points for correct picks within each bracket
  colSums(points * (bracket.picks == c(bracket.outcome)))
}
