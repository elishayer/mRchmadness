#' Compute score for bracket given actual result
#'
#' @param prediction a predicted bracket of class "bracket.filled"
#' @param result an outcome bracket of class "bracket.filled"
#' @returns score for prediction bracket if result is what actually happens
ScoreBracket = function(prediction, result) {
  sum(prediction == result)
}
