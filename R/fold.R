#' Fold a vector onto itself
#'
#' @param x A vector
#' @param block.size The size of groups in which to block the data
#' @return A new vector in the following order: first element, last element,
#'  second element, second-to-last element, ...
#' @examples
#' fold(1:64)
fold = function(x, block.size = 1) {
  if (length(x) %% block.size != 0) {
    error("length(x) / block.size must be an integer.")
  }
  num.blocks = length(x) / block.size
  blocks = split(x, rep(1:num.blocks, each = block.size))
  names(blocks) = NULL
  unlist(blocks[order(c(1:(num.blocks/2), (num.blocks/2):1))])
}
