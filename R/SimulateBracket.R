#' Simulate the full bracket starting with an empty bracket
#'
#' @param bracket - an instance of bracket.blank
#' @param bradleyTerry - an instance of matchup.prediction
#' @returns an instance of bracket.simulated
SimulateBracket = function(bracket, bradleyTerry) {
  bracket$round1 = bracket$seeds
  for (round in 1:5) {
    pairs = getPairs(bracket[[paste0('round', round)]])
    
    for (i in 1:nrow(pairs)) {
      pairs$prob[i] = bt[bt$team == pairs[i,1] & bt$opponent == pairs[i,2],]$probability
    }
    
    bracket[[paste0('round', round + 1)]] = ifelse(runif(nrow(pairs)) < pairs$prob, as.character(pairs$one), as.character(pairs$two))
  }

  class(bracket) = "bracket.simulated"
  bracket
}

#' Get the pairs of teams that will play each other in the next round
#'
#' @param vec - the vector of teams in seed order
#' @returns data.frame with the pairs of teams to play each other
getPairs = function(vec) {
  data.frame(one = vec[1:(length(vec)/2)], two = rev(vec[(length(vec)/2+1):length(vec)]), prob = rep(0, length(vec) / 2))
}
