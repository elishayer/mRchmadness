#' Fit a Bradley-Terry model on game score data
#'
#' @param game.data data.frame w/ home.id, home.score, away.id, away.score,
#' and location
#' @returns data.frame giving point spread and win probability for each matchup
#' @examples
#' data(games2017)
#' bradley.terry(games2017)
bradley.terry <- function(game.data) {

  x = Matrix::sparseMatrix(1:nrow(game.data),
    as.numeric(as.factor(game.data$home.id))) -
    Matrix::sparseMatrix(1:nrow(game.data),
    as.numeric(as.factor(game.data$away.id)))

  y = as.numeric(game.data$home.score) - as.numeric(game.data$away.score)
  
  fit = glmnet::cv.glmnet(x, y, alpha = 0, standardize = FALSE,
    lambda = exp(seq(5, -10, length = 100)))
  beta = coef(fit, s = 'lambda.min')[-1, 1]
  names(beta) = sort(unique(game.data$home.id))

  sigma = sqrt(mean((y - predict(fit, x, s = 'lambda.min'))^2))

  point.spread.matrix = beta -
    matrix(beta, nrow = length(beta), ncol = length(beta), byrow = TRUE)

  rownames(point.spread.matrix) = colnames(point.spread.matrix) = names(beta)
  pnorm(point.spread.matrix, sd = sigma)
}
