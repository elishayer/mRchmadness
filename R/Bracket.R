#' Construct blank bracket
#'
#' @param teams vector of team names, in order of overall seeding from 1 to 64
#' @return "bracket-blank" object
Bracket = function(teams) {
# Maybe this should be a constructor? Need to review those notes...
  if (length(teams) != 64) error("length(teams) must be 64.")
  bracket = list(seeds = as.character(teams))
  class(bracket) = "bracket.blank"
  bracket
}
