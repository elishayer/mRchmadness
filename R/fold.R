#' Fold a vector onto itself
#'
#' @param x a vector
#' @param block.size the size of groups in which to block the data
#' @return a new vector in the following order: first block, last block,
#'   second block, second-to-last block, ...
#' @examples
#' fold(1:64)
#' @author sspowers
fold = function(x, block.size = 1) {

  num.blocks = length(x) / block.size

# Sanitize inputs
  if (length(x) %% block.size != 0) {
    stop("length(x) / block.size must be an integer.")
  }
  if (num.blocks %% 2 != 0) {
    stop("Number of blocks must be even.")
  }

# Split by block
  blocks = split(x, rep(1:num.blocks, each = block.size))

  names(blocks) = NULL

# Fold by block
  unlist(blocks[order(c(1:(num.blocks/2), (num.blocks/2):1))])
}
