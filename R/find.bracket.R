#' Find a good bracket
#'
#' @param pool.size
#' @param num.candidates
#' @param num.sims
#' @param criterion
#' @returns 
find.bracket = function(bracket, probability.matrix, pool.size = 30,
  num.candidates = 100, num.sims = 1000,
  criterion = c("percentile", "score", "win")) {

  criterion = match.arg(criterion)

  candidates = simulate.bracket(bracket, probability.matrix,
    num.reps = num.candidates)
  pool = simulate.bracket(bracket, probability.matrix,
    num.reps = num.sims * pool.size)
  results = simulate.bracket(bracket, probability.matrix, num.reps = num.sims)
  
  score = matrix(NA, pool.size + num.candidates, num.sims)
  
  for (i in 1:num.sims) {
    brackets = cbind(pool[, (i - 1) * pool.size + 1:pool.size], candidates)
    score[, i] = score.bracket(results[, i], brackets)
  }

  if (criterion == "percentile") {
    rank = apply(score[-(1:pool.size), ], 2, rank, ties.method = 'max')
    percentile = rank / nrow(score)
    return(candidates[, which.max(rowMeans(percentile))])
  }

  if (criterion == "score") {
    return(candidates[, which.max(rowMeans(score[-(1:pool.size), ]))])
  }

  if (criterion == "win") {
    win = score[-(1:pool.size), ] >= apply(score[1:pool.size, ], 2, max)
    return(candidates[, which.max(rowMeans(win))])
  }
}  
