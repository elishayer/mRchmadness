#' Fit a Bradley-Terry model on game score data
#'
#' @param games data.frame with the following columns: game.id, home.id,
#'   away.id, home.score, away.score, neutral, ot (matched by output of
#'   scrape.game.results)
#' @returns matrix of win probabilities, with rows and columns labeled by team.
#'   Each entry gives the probability of the team corresponding to that row
#'   beating the team corresponding to that column.
#' @author sspowers
bradley.terry <- function(games) {

  columns = c("game.id", "home.id", "away.id", "home.score", "away.score",
    "neutral", "ot")

# Check that input is data.frame with necessary columns
  if (!("data.frame" %in% class(games)) |
    length(setdiff(columns, names(games))) > 0) {
    stop(paste("games much be 'data.frame' with columns", columns))
  }

# Construct design matrix for Bradley-Terry model: +1 for home, -1 for away
  x = Matrix::sparseMatrix(1:nrow(games),
    as.numeric(as.factor(games$home.id))) -
    Matrix::sparseMatrix(1:nrow(games),
    as.numeric(as.factor(games$away.id)))

# Build response vector: home score minus away score
  y = as.numeric(games$home.score) - as.numeric(games$away.score)

# Fit model via ridge regression
# Choose lambda to minimize cross-validation error
  fit = glmnet::cv.glmnet(x, y, alpha = 0, standardize = FALSE,
    lambda = exp(seq(5, -10, length = 100)))
  beta = coef(fit, s = 'lambda.min')[-1, 1]
  names(beta) = sort(unique(games$home.id))

# Estimate variance in score differential
  sigma = sqrt(mean((y - predict(fit, x, s = 'lambda.min'))^2))

# Get estimated point spread for each possible matchup
  point.spread.matrix = beta -
    matrix(beta, nrow = length(beta), ncol = length(beta), byrow = TRUE)

# Name the rows and columns of the matrix according to corresponding team
  rownames(point.spread.matrix) = colnames(point.spread.matrix) = names(beta)

# Convert point spreads into probability of winning
  pnorm(point.spread.matrix, sd = sigma)
}
