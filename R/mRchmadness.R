#' March Madness Bracket Package
#'
#' \code{mRchmadness} provides utilities to gather NCAA Men's
#' Basketball data, predict the win probability of matchups,
#' and produce a bracket optimized to a specified criterion
#'
#' @importFrom utils globalVariables
#' @docType package
#' @name mRchmadness
NULL

utils::globalVariables(names = c('location',
                                 'other.id',
                                 'primary.id',
                                 'game.id',
                                 'home',
                                 'primary.score',
                                 'other.score',
                                 'ot',
                                 'home.id',
                                 'away.id',
                                 '.'))

