#' Fit a Bradley-Terry model on game score data
#'
#' @param game.data data.frame w/ home, home.score, away, away.score, location
#' @returns data.frame giving point spread and win probability for each matchup
#' @examples
#' data(games2017)
#' BradleyTerr(games2017)
BradleyTerry <- function(game.data) {

  x = Matrix::sparseMatrix(1:nrow(game.data),
    as.numeric(as.factor(game.data$home))) -
    Matrix::sparseMatrix(1:nrow(game.data),
    as.numeric(as.factor(game.data$away)))

  y = as.numeric(game.data$home.score) - as.numeric(game.data$away.score)
  
  fit = glmnet::cv.glmnet(x, y, alpha = 0, standardize = FALSE,
    lambda = exp(seq(5, -10, length = 100)))
  beta = coef(fit, s = 'lambda.min')[-1, 1]
  names(beta) = sort(unique(game.data$home))

  sigma = sqrt(mean((y - predict(fit, x, s = 'lambda.min'))^2))

  point.spread.matrix = beta -
    matrix(beta, nrow = length(beta), ncol = length(beta), byrow = TRUE)

  rownames(point.spread.matrix) = colnames(point.spread.matrix) = names(beta)

  point.spread.matrix %>% as.data.frame %>%
    mutate(team = names(beta)) %>%
    gather(key = opponent, value = spread, -team) %>%
    filter(team != opponent) %>%
    mutate(probability = 1 - pnorm(0, mean = spread, sd = sigma))
}
