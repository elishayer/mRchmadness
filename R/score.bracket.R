#' Compute score for bracket given actual result
#'
#' @param bracket.empy
#' @param bracket.picks
#' @param bracket.outcome
#' @param bonus.round
#' @param bonus.seed
#' @param bonus.combine
#' @returns 
score.bracket = function(bracket.empty, bracket.picks, bracket.outcome,
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

  bonus.combine = match.arg(bonus.combine)

  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)
  round.bonus = bonus.round[round]

  seed = rep(1:16, each = 4)
  names(seed) = bracket.empty
  seed.bonus = matrix(bonus.seed[seed[bracket.picks]], 63, ncol(bracket.picks))

  bonus = switch(bonus.combine,
    add = round.bonus + seed.bonus,
    multiply = round.bonus * seed.bonus)

  colSums(bonus * (bracket.picks == c(bracket.outcome)))
}
