#' Test a bracket
#'
#' @param bracket.empy
#' @param probability.matrix
#' @param my.bracket
#' @param pool.size
#' @param num.sims
#' @param bonus.round
#' @param bonus.seed
#' @param bonus.combine
#' @author sspowers
test.bracket = function(bracket.empty, probability.matrix, my.bracket,
  pool.size = 30, num.sims = 1000,
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

  num.sims = 1000
  pool.size = 30
  
  pool = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims * pool.size)
  outcome = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims)
  
  score = matrix(NA, pool.size + 1, num.sims)
  
  for (i in 1:num.sims) {
    brackets = cbind(my.bracket, pool[, (i - 1) * pool.size + 1:pool.size])
    score[, i] = score.bracket(bracket.empty = bracket.empty,
      bracket.picks = brackets, bracket.outcome = outcome[, i],
      bonus.round = bonus.round, bonus.seed = bonus.seed,
      bonus.combine = bonus.combine)
  }
  
  rank = apply(score, 2, rank, ties.method = 'max')
  percentile = rank[1, ] / nrow(score)

  win = score[1, ] == apply(score, 2, max)

  list(score = score[1, ], percentile = percentile, win = win)
}
