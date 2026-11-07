#' Test whether a matrix is a transposition matrix
#'
#' Checks that a matrix has the form T_ij = I_n - E_ii - E_jj + E_ij + E_ji,
#' with i != j , i.e. the identity matrix with two rows swapped.
#' The identity matrix is not a transposition matrix.
#'
#' @param M A matrix.
#' @return \code{TRUE} or \code{FALSE}.
#' @examples
#' est_transposition(matrice_T(4, 1, 3))
#' est_transposition(matrix(1:4, 2))
#' @export
est_transposition <- function(M) {
  if (!is.matrix(M) || !is.numeric(M) || anyNA(M) ||
      nrow(M) != ncol(M) || nrow(M) < 1) {
    return(FALSE)
  }
  n <- nrow(M)
  diff_rows <- which(rowSums(M != diag(n)) > 0)
  if (length(diff_rows) != 2) return(FALSE)
  isTRUE(all.equal(M, matrice_T(n, diff_rows[1], diff_rows[2])))
}

#' Properties of a transposition matrix
#'
#' Checks the classical properties of T_ij: symmetric, orthogonal,
#' involutive (T_ij %*% T_ij = I_n) and its determinant.
#'
#' @param n Size of the matrix.
#' @param i,j Indices of the two rows to swap.
#' @return A list with \code{symetrique}, \code{orthogonale},
#'   \code{involutive} (logical) and \code{determinant}
#'   (-1 if i != j, 1 if i = j).
#' @examples
#' proprietes_T(4, 1, 3)
#' @export
proprietes_T <- function(n, i, j) {
  Tm <- matrice_T(n, i, j)
  In <- diag(n)
  list(
    symetrique  = isTRUE(all.equal(Tm, t(Tm))),
    orthogonale = isTRUE(all.equal(Tm %*% t(Tm), In)),
    involutive  = isTRUE(all.equal(Tm %*% Tm, In)),
    determinant = round(det(Tm))
  )
}

#' Apply several swaps in a row
#'
#' Applies a sequence of transpositions to the rows (or columns)
#' of a matrix. The result is a permutation of the rows (or columns).
#'
#' @param A A matrix.
#' @param paires A list of pairs of indices,
#'   e.g. \code{list(c(1, 3), c(2, 3))}.
#' @param cote Either "rows" (default) or "columns".
#' @return The matrix A after all swaps, applied in the given order.
#' @examples
#' A <- matrix(1:6, nrow = 3)
#' echanges(A, list(c(1, 3), c(2, 3)))
#' @export
echanges <- function(A, paires, cote = c("rows", "columns")) {
  cote <- match.arg(cote)
  if (!is.list(paires) || any(lengths(paires) != 2)) {
    stop("'paires' must be a list of pairs of indices", call. = FALSE)
  }
  A <- as.matrix(A)
  for (p in paires) {
    A <- echange(A, p[1], p[2], cote)
  }
  A
}
