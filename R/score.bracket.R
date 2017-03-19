#' Compute score for bracket given actual result
#'
#' @param brackets a predicted bracket of class "bracket.filled"
#' @param results an outcome bracket of class "bracket.filled"
#' @returns score for prediction bracket if result is what actually happens
score.bracket = function(brackets, results) {
  colSums(brackets == results)
}
