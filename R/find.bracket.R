#' Find a good bracket
#'
#' @param bracket.empty
#' @param num.candidates
#' @param num.sims
#' @param criterion
#' @returns
#' @author sspowers
find.bracket = function(bracket.empty, probability.matrix, pool.size = 30,
  num.candidates = 100, num.sims = 1000,
  criterion = c("percentile", "score", "win"),
  bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
  bonus.combine = c("add", "multiply")) {

  criterion = match.arg(criterion)

  candidates = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.candidates)
  pool = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims * pool.size)
  outcome = simulate.bracket(bracket.empty, probability.matrix,
    num.reps = num.sims)
  
  score = matrix(NA, pool.size + num.candidates, num.sims)
  
  for (i in 1:num.sims) {
    brackets = cbind(pool[, (i - 1) * pool.size + 1:pool.size], candidates)
    score[, i] = score.bracket(bracket.empty = bracket.empty, 
      bracket.picks = brackets, bracket.outcome = outcome[, i],
      bonus.round = bonus.round, bonus.seed = bonus.seed,
      bonus.combine = bonus.combine)
  }

# Find bracket with highest average percentile finish
  if (criterion == "percentile") {
    rank = apply(score[-(1:pool.size), ], 2, rank, ties.method = 'max')
    percentile = rank / nrow(score)
    return(candidates[, which.max(rowMeans(percentile))])
  }

# Find bracket with highest average score
  if (criterion == "score") {
    return(candidates[, which.max(rowMeans(score[-(1:pool.size), ]))])
  }

# Find bracket which wins most
  if (criterion == "win") {
    win = score[-(1:pool.size), ] >= apply(score[1:pool.size, ], 2, max)
    return(candidates[, which.max(rowMeans(win))])
  }
}  
