#' Test a bracket
#'
test.bracket = function(bracket, probability.matrix, my.bracket,
  pool.size = 30, num.sims = 1000) {

  num.sims = 1000
  pool.size = 30
  
  pool = simulate.bracket(bracket, probability.matrix,
    num.reps = num.sims * pool.size)
  results = simulate.bracket(bracket, probability.matrix, num.reps = num.sims)
  
  score = matrix(NA, pool.size + 1, num.sims)
  
  for (i in 1:num.sims) {
    brackets = cbind(my.bracket, pool[, (i - 1) * pool.size + 1:pool.size])
    score[, i] = score.bracket(results[, i], brackets)
  }
  
  rank = apply(score, 2, rank, ties.method = 'max')
  percentile = rank[1, ] / nrow(score)

  win = score[1, ] == apply(score, 2, max)

  list(score = score[1, ], percentile = percentile, win = win)
}
