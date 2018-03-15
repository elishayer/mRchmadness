#' Reorder from pool-order to the internal order
#'
#' @param bracket.picks a length-63 character vector giving the 63 game
#'   outcomes in the order from reading off a standard-formatted bracket. More
#'   specifically, this is the first 32 first-round games going by region from
#'   South to West to East to Midwest, and within region the winner of the game
#'   between, in order, 1-16, 8-9, 5-12, 4-13, 6-11, 3-14, 7-10, 2-15. Then the
#'   winners of the second round in the same region order, and the winner of
#'   1-16-8-9, 5-12-4-13, 6-11-3-14, and 7-10-2-15. At this point the pattern
#'   should be clear. See https://github.com/elishayer/mRchmadness/issues/23
#'   for more details.
#' @return a vector giving the picked bracket in the way used elsewhere
#'   in mRchmadness (see sim.bracket documentation)
#' @export
#' @author elishayer
input.bracket.filled = function(bracket.picks) {

  if (length(bracket.picks) != 63) {
    stop("Length of bracket.picks must be 63.")
  }

  # This is a magic vector, of sorts. See #23 for derivation.
  reorder = c(1, 17, 25, 9, 13, 29, 21, 5, 7, 23, 31, 15, 11, 27, 19, 3, 4,
    20, 28, 12, 16, 32, 24, 8, 6, 22, 30, 14, 10, 26, 18, 2, 33, 41, 45, 37,
    39, 47, 43, 35, 36, 44, 48, 40, 38, 46, 42, 34, 49, 53, 55, 51, 52, 56,
    54, 50, 57, 59, 60, 58, 61, 62, 63)

  bracket.picks[reorder]
}
