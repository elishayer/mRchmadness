#' Plot bracket to device
#'
#' @param bracket.empty a length-64 character vector giving the field of 64
#'   teams in the tournament, in order of initial overall seeding
#' @param bracket.filled an optional length-63 character vector encoding
#'   tournament results (matching output from simulate.bracket)
#' @param league which league: "men" (default) or "women".
#'   Used for converting team IDs into team names
#' @param text.size character expansion factor for teams written into bracket
#' @examples
#'   prob.matrix = bradley.terry(games = games.men.2018)
#'   outcome = sim.bracket(bracket.empty = bracket.men.2018,
#'     prob.matrix = prob.matrix)
#'   draw.bracket(bracket.empty = bracket.men.2018, bracket.filled = outcome)
#' @export
#' @author sspowers
draw.bracket = function(bracket.empty, bracket.filled = NULL,
  league = c("men", "women"), text.size = 0.5) {

  `%>%` = dplyr::`%>%`

# Sanitize inputs
  league = match.arg(league)
  if (length(bracket.empty) != 64) {
    stop("Length of bracket.empty must be 64.")
  }
  if (!is.null(bracket.filled) && length(bracket.filled) != 63) {
    stop("Length of bracket.empty (if specified) must be 63.")
  }

# Load the dataframe of teams corresponding to the correct league
  teams = eval(parse(text = paste("mRchmadness::teams", league, sep = ".")))
# Convert team IDs into names
  team.names = teams$name
  names(team.names) = teams$id
  bracket.empty = bracket.empty %>%
    # Handle unplayed first-round games signified by "/"
    strsplit(split = '/') %>%
    lapply(function(id) as.character(team.names[id])) %>%
    sapply(function(names) do.call(paste, args = as.list(c(names, sep = '/'))))
  if (!is.null(bracket.filled)) {
    # Only handle unplayed first-round games if test bracket is not NULL
    bracket.filled = bracket.filled %>%
      strsplit(split = '/') %>%
      lapply(function(id) as.character(team.names[id])) %>%
      sapply(function(names) {
        do.call(paste, args = as.list(c(names, sep = '/')))})
  }

# append "seed" to beginning of team names
  seed = rep(1:16, each = 4)
  names(seed) = bracket.empty

# convert initial team placement from seed order to matchup order
  bracket.empty = paste(seed, bracket.empty) %>% fold(1) %>% fold(2) %>%
    fold(4) %>% fold(8) %>% fold(16) %>% fold(32)

  if (!is.null(bracket.filled)) {
# convert each round of results from seed order to matchup order
    bracket.filled = paste(seed[bracket.filled], bracket.filled)
    round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)
    bracket.filled[round == 1] = bracket.filled[round == 1] %>% fold(1) %>%
      fold(2) %>% fold(4) %>% fold(8) %>% fold(16)
    bracket.filled[round == 2] = bracket.filled[round == 2] %>% fold(1) %>%
      fold(2) %>% fold(4) %>% fold(8)
    bracket.filled[round == 3] = bracket.filled[round == 3] %>% fold(1) %>%
      fold(2) %>% fold(4)
    bracket.filled[round == 4] = bracket.filled[round == 4] %>% fold(1) %>%
      fold(2)
    bracket.filled[round == 5] = bracket.filled[round == 5] %>% fold(1)
  }

# x and y coordinates for centers of all horizontal lines in bracket
  x = c(rep(-6, 32), rep(6, 32), rep(-5, 16), rep(5, 16), rep(-4, 8),
    rep(4, 8), rep(-3, 4), rep(3, 4), c(-2, -2), 2, 2, -1, 1, 0)
  y = c(rep(seq(63/64, 1/64, -1/32), 2), rep(seq(31/32, 1/32, -1/16), 2),
    rep(seq(15/16, 1/16, -1/8), 2), rep(seq(7/8, 1/8, -1/4), 2),
    rep(c(3/4, 1/4), 2), 3/5, 2/5, 1/2)

  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot(NA, xlim = c(-7, 7), ylim = 0:1, xlab = '', ylab = '',
    axes = FALSE)
# horizontal line segments
  graphics::segments(x - 1/2, y, x + 1/2, y)
# vertical line segments
  graphics::segments((x + (x < 0) - 1/2)[seq(1, length(x) - 3, 2)],
    y[seq(1, length(y) - 3, 2)],
    (x + (x < 0) - 1/2)[seq(2, length(x) - 3, 2)],
    y[seq(2, length(y) - 3, 2)])

# fill in intial seeding
  graphics::text(x[1:64] - 0.46, y[1:64] + 0.01, bracket.empty,
    cex = text.size, adj = 0)

  if (!is.null(bracket.filled)) { # fill in tournament results
    graphics::text(x[-(1:64)] - 0.46, y[-(1:64)] + 0.01, bracket.filled,
      cex = text.size, adj = 0)
  }
}
