#' Unfold a vector (the inverse of the fold function)
#'
#' @param x a vector
#' @param block.size the size of groups in which to block the data
#' @return a vector in the following order: block 1, block 3, ..., block n-1,
#'   block n, block n-2, ..., block 2.
#' @examples
#' unfold(fold(1:64))
#' @author sspowers
unfold = function(x, block.size = 1) {

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

# Unfold by block
  unlist(blocks[c(seq(1, num.blocks - 1, 2), seq(num.blocks, 2, -2))])
}
