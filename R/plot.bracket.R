#' Plot bracket to device
#'
#' @param bracket vector of length 64 giving initial tournament seeding
#' @param filling optional vector of length 63 giving tournament results
#' @examples
plot.bracket = function(bracket, filling = NULL) {

# append "seed" to beginning of team names
  seed = rep(1:16, each = 4)
  names(seed) = bracket

# convert initial team placement from seed order to matchup order
  bracket = paste(seed, bracket) %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16) %>% fold(32)

  if (!is.null(filling)) {
# convert each round of results from seed order to matchup order
    filling = paste(seed[filling], filling)
    round = c(rep(1, 32), rep(2, 16), rep(3, 8), rep(4, 4), 5, 5, 6)
    filling[round == 1] = filling[round == 1] %>% fold(1) %>% fold(2) %>%
      fold(4) %>% fold(8) %>% fold(16)
    filling[round == 2] = filling[round == 2] %>% fold(1) %>% fold(2) %>%
      fold(4) %>% fold(8)
    filling[round == 3] = filling[round == 3] %>% fold(1) %>% fold(2) %>%
      fold(4)
    filling[round == 4] = filling[round == 4] %>% fold(1) %>% fold(2)
    filling[round == 5] = filling[round == 5] %>% fold(1)
  }

# x and y coordinates for centers of all horizontal lines in bracket
  x = c(rep(-6, 32), rep(6, 32), rep(-5, 16), rep(5, 16), rep(-4, 8),
    rep(4, 8), rep(-3, 4), rep(3, 4), c(-2, -2), 2, 2, -1, 1, 0)
  y = c(rep(seq(63/64, 1/64, -1/32), 2), rep(seq(31/32, 1/32, -1/16), 2),
    rep(seq(15/16, 1/16, -1/8), 2), rep(seq(7/8, 1/8, -1/4), 2),
    rep(c(3/4, 1/4), 2), 3/5, 2/5, 1/2)

  plot(NA, xlim = c(-7, 7), ylim = 0:1, xlab = '', ylab = '', axes = FALSE)
# horizontal line segments
  segments(x - 1/2, y, x + 1/2, y)
# vertical line segments
  segments((x + (x < 0) - 1/2)[seq(1, length(x) - 3, 2)],
    y[seq(1, length(y) - 3, 2)],
    (x + (x < 0) - 1/2)[seq(2, length(x) - 3, 2)],
    y[seq(2, length(y) - 3, 2)])

# fill in intial seeding
  text(x[1:64] - 0.46, y[1:64] + 0.01, bracket, cex = 0.4, adj = 0)

  if (!is.null(filling)) { # fill in tournament results
    text(x[-(1:64)] - 0.46, y[-(1:64)] + 0.01, filling, cex = 0.4, adj = 0)
  }
}
