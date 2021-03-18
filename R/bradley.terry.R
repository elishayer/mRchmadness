#' Fit a Bradley-Terry model on game score data
#'
#' @param games data.frame with the following columns: game.id, home.id,
#'   away.id, home.score, away.score, neutral, ot (matched by output of
#'   scrape.game.results)
#' @return matrix of win probabilities, with rows and columns labeled by team.
#'   Each entry gives the probability of the team corresponding to that row
#'   beating the team corresponding to that column.
#' @examples
#'   prob = bradley.terry(games = games.men.2018)
#' @export
#' @author sspowers
bradley.terry = function(games) {

  columns = c("game.id", "home.id", "away.id", "home.score", "away.score",
    "neutral", "ot")

# Check that input is data.frame with necessary columns
  if (!("data.frame" %in% class(games)) |
    length(setdiff(columns, names(games))) > 0) {
    stop(paste("games much be 'data.frame' with columns", columns))
  }

  games = dplyr::filter(games, home.id != "NA" & away.id != "NA")

# Construct design matrix for Bradley-Terry model:
# First column: 0 if neutral location, 1 otherwise
# All other columns correspond to teams --- +1 in row means that team is home
# for corresponding game, -1 means that team is away.
  x = Matrix::sparseMatrix(
    i = c(which(games$neutral == 0), rep(1:nrow(games), 2)),
    j = c(rep(1, sum(games$neutral == 0)),
      1 + as.numeric(as.factor(c(games$home.id, games$away.id)))),
    x = c(rep(1, sum(games$neutral == 0)), rep(c(1, -1), each = nrow(games))))

# Build response vector: home score minus away score (0 if game went overtime)
  y = as.numeric(games$home.score) - as.numeric(games$away.score)
  y[games$ot != ""] = 0

# Fit model via ridge regression
# Choose lambda to minimize cross-validation error
  fit = glmnet::cv.glmnet(x, y, alpha = 0, standardize = FALSE,
    intercept = FALSE, lambda = exp(seq(5, -10, length = 100)))
  beta = stats::coef(fit, s = 'lambda.min')[-1, 1]
  names(beta) = c("home", sort(unique(games$home.id)))

# Estimate variance in score differential
  sigma = sqrt(mean((y - stats::predict(fit, x, s = 'lambda.min'))^2))

# Get estimated point spread for each possible matchup
  point.spread.matrix = beta[-1] - matrix(beta[-1], nrow = length(beta) - 1,
    ncol = length(beta) - 1, byrow = TRUE)

# Name the rows and columns of the matrix according to corresponding team
  rownames(point.spread.matrix) = colnames(point.spread.matrix) =
    names(beta)[-1]

# Convert point spreads into probability of winning
  stats::pnorm(point.spread.matrix, sd = sigma)
}
