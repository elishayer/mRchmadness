#' Fit a Bradley-Terry model on game score data
#'
#' @param game.data data.frame w/ home, home.score, away, away.score, location
#' @returns
BradleyTerry <- function(game.data) {
  x = Matrix::sparseMatrix(1:nrow(games), as.numeric(as.factor(games$home))) -
  Matrix::sparseMatrix(1:nrow(games), as.numeric(as.factor(games$away)))

  y = as.numeric(games$home.score) - as.numeric(games$away.score)
  
  fit = glmnet::cv.glmnet(x, y, alpha = 0, standardize = FALSE,
    lambda = exp(seq(5, -10, length = 100)))
  beta = coef(fit, s = 'lambda.min')[, 1]
  names(beta) = c('intercept', sort(unique(games$home)))
  
}
