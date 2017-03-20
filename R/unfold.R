#' Unfold a vector (the inverse of the fold function)
#'
#' @param x A vector
#' @param block.size The size of groups in which to block the data
#' @examples
#' unfold(fold(1:64))
#' @author sspowers
unfold = function(x, block.size = 1) {
  if (length(x) %% block.size != 0) {
    error("length(x) / block.size must be an integer.")
  }
  num.blocks = length(x) / block.size
  blocks = split(x, rep(1:num.blocks, each = block.size))
  names(blocks) = NULL
  unlist(blocks[c(seq(1, num.blocks - 1, 2), seq(num.blocks, 2, -2))])
}
