# find_bracket_edit_2

bracket.empty = bracket.men.2021
prob.matrix = prob.matrix.21
current.year = 2021
num.candidates = 100
num.sims = 1000
criterion = "win"
pool.size = 30
bonus.round = c(1, 2, 4, 8, 16, 32)
bonus.seed = rep(0, 16)
bonus.combine = c("add", "multiply")
prob.source = NULL
pool.source = "pop"
year = 2021
league = "men"
pool.bias = NULL
num.reps = 1

find.bracket22 = function(bracket.empty, prob.matrix = NULL,
                        prob.source = c("pop", "kenpom", "538"),
                        pool.source = c("pop", "kenpom", "538"), league = c("men", "women"),
                        year = current.year, pool.bias = NULL, num.candidates = 100,
                        num.sims = 1000, criterion = c("percentile", "score", "win"),
                        pool.size = 30,
                        bonus.round = c(1, 2, 4, 8, 16, 32), bonus.seed = rep(0, 16),
                        bonus.combine = c("add", "multiply"),
                        print.progress = TRUE, shiny.progress = NULL) {
  
  criterion = match.arg(criterion)
  
  # Input sanitization
  if (!is.numeric(bonus.round) | length(bonus.round) != 6) {
    stop("bonus.round must be length-6 numeric vector")
  }
  if (!is.numeric(bonus.seed) | length(bonus.seed) != 16) {
    stop("bonus.seed must be length-16 numeric vector")
  }
  num.candidates = as.integer(num.candidates)
  if (num.candidates < 2) {
    stop("num.candidates must be at least 2")
  }
  num.sims = as.integer(num.sims)
  if (num.sims < 2) {
    stop("num.sims must be at least 2")
  }
  pool.size = as.integer(pool.size)
  if (pool.size < 1) {
    stop("pool.size must be at least 1")
  }
  if("kenpom" %in% c(prob.source, pool.source) && (league == "women")) {
    stop("kenpom is only available for the men's bracket.")
  }
  
  # Simulate the brackets to be considered
  candidates = sim.bracket22(bracket.empty = bracket.empty,
                           prob.matrix = prob.matrix, prob.source = prob.source, league = league,
                           year = year, num.reps = num.candidates)
  
  # # Update the user on progress
  # if (print.progress) {
  #   cat('Finding your bracket ...')
  #   cat('\n  Simulating', num.sims, 'pools of size', pool.size, '...')
  # }
  # if (!is.null(shiny.progress)) {
  #   shiny.progress$set(detail =
  #                        paste('Simulating', num.sims, 'pools of size', pool.size))
  # }
  
  # Simulate all of the pools (across all simulations)
  pool = sim.bracket22(bracket.empty = bracket.empty, prob.matrix = prob.matrix.21,
                       prob.source = NULL,
                     league = league, year = year, home.teams = pool.bias,
                     num.reps = num.sims * pool.size)
  
  # Simulate all of the outcomes
  outcome = sim.bracket22(bracket.empty = bracket.empty,
                        prob.matrix = prob.matrix, prob.source = prob.source, league = league,
                        year = year, num.reps = num.sims)
  
  # Prepare matrix to store all bracket scores
  score = matrix(NA, pool.size + num.candidates, num.sims)
  
  # Update the user on progress
  if (print.progress) {
    cat('\n  Scoring', num.sims * (num.candidates + pool.size), 'brackets ...',
        '\n    ') 
  }
  if (!is.null(shiny.progress)) {
    shiny.progress$set(detail =
                         paste('Scoring', num.sims * (num.candidates + pool.size), 'brackets'))
  }
  
  for (i in 1:num.sims) {
    # Extract brackets to be evaluated on this simulation
    brackets = cbind(pool[, (i - 1) * pool.size + 1:pool.size], candidates)
    # Score all of these brackets against outcome of this simulation
    score[, i] = mRchmadness:::score.bracket(bracket.empty = bracket.empty, 
                               bracket.picks = brackets, bracket.outcome = outcome[, i],
                               bonus.round = bonus.round, bonus.seed = bonus.seed,
                               bonus.combine = bonus.combine)
    # # Update the user on progress
    # if (print.progress & (i %% 1000) == 0) {
    #   cat(paste0(round(100 * i / num.sims), '% '))
    # }
    # if (!is.null(shiny.progress)) shiny.progress$set(value = i)
  }
  if (print.progress) cat('\n')
  
  # Find bracket with highest average percentile finish (compare only to pool)
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
    win = score[-(1:pool.size), ] >=
      apply(score[1:pool.size, , drop = FALSE], 2, max)
    return(candidates[, which.max(rowMeans(win))])
  }
}
