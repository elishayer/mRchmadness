#' Plot blank bracket to device
#'
#' @param bracket object of class "bracket.blank"
#' @examples
#' data(bracket2017)
#' plot(bracket2017)
plot.bracket.blank = function(bracket) {
  seed = rep(1:16, each = 4) %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16)
  team = bracket$seeds %>% fold(1) %>% fold(2) %>% fold(4) %>%
    fold(8) %>% fold(16)
  plot(NA, xlim = c(0, 12), ylim = c(1, 33) - 16.5, axes = FALSE,
    xlab = '', ylab = '')
  segments(rep(0, 32), 1:32 - 16.5, rep(1, 32), 1:32 - 16.5)
  segments(rep(12, 32), 1:32 - 16.5, rep(11, 32), 1:32 - 16.5)
  segments(rep(1, 16), 2*(1:16 - 8.5), rep(2, 16), 2*(1:16 - 8.5))
  segments(rep(11, 16), 2*(1:16 - 8.5), rep(10, 16), 2*(1:16 - 8.5))
  segments(rep(2, 8), 4*(1:8 - 4.5), rep(3, 8), 4*(1:8 - 4.5))
  segments(rep(10, 8), 4*(1:8 - 4.5), rep(9, 8), 4*(1:8 - 4.5))
  segments(rep(3, 4), 8*(1:4 - 2.5), rep(4, 4), 8*(1:4 - 2.5))
  segments(rep(9, 4), 8*(1:4 - 2.5), rep(8, 4), 8*(1:4 - 2.5))
  segments(rep(4, 2), 16*(1:2 - 1.5), rep(5, 2), 16*(1:2 - 1.5))
  segments(rep(8, 2), 16*(1:2 - 1.5), rep(7, 2), 16*(1:2 - 1.5))
  segments(5, 4, 6, 4)
  segments(7, -4, 6, -4)
  segments(5.5, 0, 6.5, 0)
  segments(1, seq(1, 32, by = 2) - 16.5, 1, seq(2, 32, by = 2) - 16.5)
  segments(11, seq(1, 32, by = 2) - 16.5, 11, seq(2, 32, by = 2) - 16.5)
  segments(2, 2*(seq(1, 16, by = 2) - 8.5), 2, 2*(seq(2, 16, by = 2) - 8.5))
  segments(10, 2*(seq(1, 16, by = 2) - 8.5), 10, 2*(seq(2, 16, by = 2) - 8.5))
  segments(3, 4*(seq(1, 8, by = 2) - 4.5), 3, 4*(seq(2, 8, by = 2) - 4.5))
  segments(9, 4*(seq(1, 8, by = 2) - 4.5), 9, 4*(seq(2, 8, by = 2) - 4.5))
  segments(4, 8*(seq(1, 4, by = 2) - 2.5), 4, 8*(seq(2, 4, by = 2) - 2.5))
  segments(8, 8*(seq(1, 4, by = 2) - 2.5), 8, 8*(seq(2, 4, by = 2) - 2.5))
  segments(5, 16*(seq(1, 2, by = 2) - 1.5), 5, 16*(seq(2, 2, by = 2) - 1.5))
  segments(7, 16*(seq(1, 2, by = 2) - 1.5), 7, 16*(seq(2, 2, by = 2) - 1.5))
  text(rep(c(0, 11), each = 32), rep(32:1 - 16.5, 2) + 0.3,
    labels = paste(seed, team), cex = 0.5, adj = 0)
}
