#' Transpose a matrix with transposition matrices
#'
#' Rows become columns. The entries of A, stacked column by column, are
#' rearranged by a sequence of swaps, each one a product by a matrix T_ij.
#' The result is the transpose of A.
#'
#' @param A A matrix (or an object coercible to a matrix).
#' @param method \code{"swaps"} (default) uses \code{\link{matrice_T}} and
#'   is meant for small matrices (at most 400 entries). \code{"direct"}
#'   uses \code{t(A)} and works for any size.
#' @return The transposed matrix: a p x m matrix if A is m x p.
#' @examples
#' A <- matrix(1:6, nrow = 2)
#' A
#' transposer(A)
#' @export
transposer <- function(A, method = c("swaps", "direct")) {
  method <- match.arg(method)
  A <- as.matrix(A)
  if (method == "direct") {
    return(t(A))
  }
  m <- nrow(A)
  p <- ncol(A)
  N <- m * p
  if (N > 400) {
    stop("method 'swaps' is meant for matrices with at most 400 entries; ",
         "use method = \"direct\"", call. = FALSE)
  }
  x <- c(A)
  i <- rep(seq_len(m), each = p)
  j <- rep(seq_len(p), times = m)
  q <- (j - 1) * m + i
  current <- seq_len(N)
  for (t in seq_len(N)) {
    pos <- which(current == q[t])
    if (pos != t) {
      x <- drop(matrice_T(N, t, pos) %*% x)
      current[c(t, pos)] <- current[c(pos, t)]
    }
  }
  matrix(x, nrow = p, ncol = m)
}
