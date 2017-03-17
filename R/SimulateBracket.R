#' Simulate the full bracket starting with an empty bracket
#'
#' @param bracket an instance of bracket.blank
#' @param probability.matrix output from BradleyTerry
#' @returns an instance of bracket.simulated
SimulateBracket = function(bracket, probability.matrix, num.reps) {

  result = matrix('', 63, num.reps)
  round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), rep(5, 2), 6)
  teams = bracket$seeds %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16) %>% fold(32) %>% rep(num.reps)

  # The following chunk of code is a currently necessary evil.
  # To simulate quickly, teams need to be in matchup order, but we want to
  # store the results in seed order within each round.
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

  for (r in 1:6) {
    matchups = matrix(teams, nrow = 2^(6 - r) * num.reps, ncol = 2,
      byrow = TRUE)
    teams = matchups[1:nrow(matchups) + nrow(matchups) *
      (1 - rbinom(nrow(matchups), 1, probability.matrix[matchups]))]
    result[round == r, ] = matrix(teams, nrow = 2^(6 - r),
      ncol = num.reps)[untangling.indices[[r]], ]
  }
  bracket = list(winners = result)
  class(bracket) = "bracket.simulated"
  bracket
}

##' Get the pairs of teams that will play each other in the next round
##'
##' @param vec - the vector of teams in seed order
##' @returns data.frame with the pairs of teams to play each other
#getPairs = function(vec) {
#  data.frame(one = vec[1:(length(vec)/2)], two = rev(vec[(length(vec)/2+1):length(vec)]), prob = rep(0, length(vec) / 2))
#}
