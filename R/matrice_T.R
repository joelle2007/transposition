#' Transposition matrix T_ij
#'
#' Builds the matrix T_ij = I_n - E_ii - E_jj + E_ij + E_ji,
#' which is the identity matrix with rows i and j swapped.
#'
#' @param n Positive integer. Size of the square matrix.
#' @param i,j Integers between 1 and n (i != j). Positions to swap.
#' @return An n x n numeric matrix.
#' @examples
#' matrice_T(3, 1, 3)
#' @export
matrice_T <- function(n, i, j) {
  verif_indice(n, "n")
  verif_indice(i, "i", n)
  verif_indice(j, "j", n)
  if (i == j) {
    stop("'i'and 'j'must be different (i !=j)",call. = FALSE)
  }

  E <- function(a, b) {
    M <- matrix(0, n, n)
    M[a, b] <- 1
    M
  }
  diag(n) - E(i, i) - E(j, j) + E(i, j) + E(j, i)
}

#' Swap two rows or two columns of a matrix
#'
#' Works for square and non-square matrices. Rows are swapped with
#' T_ij %*% A, columns with A %*% T_ij.
#'
#' @param A A matrix (or an object coercible to a matrix).
#' @param i,j Indices of the two rows (or columns) to swap.
#' @param cote Either "rows" (default) or "columns".
#' @return The matrix A with rows (or columns) i and j swapped.
#' @examples
#' A <- matrix(1:6, nrow = 3)
#' echange(A, 1, 3)
#' echange(A, 1, 2, "columns")
#' @export
echange <- function(A, i, j, cote = c("rows", "columns")) {
  cote <- match.arg(cote)
  A <- as.matrix(A)
  dn <- dimnames(A)
  res <- if (cote == "rows") {
    matrice_T(nrow(A), i, j) %*% A
  } else {
    A %*% matrice_T(ncol(A), i, j)
  }
  if (!is.null(dn)) {
    k <- if (cote == "rows") 1 else 2
    if (!is.null(dn[[k]])) dn[[k]][c(i, j)] <- dn[[k]][c(j, i)]
    dimnames(res) <- dn
  }
  res
}


# Internal helper (not exported)
verif_indice <- function(x, nom, max = Inf) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) ||
      x != round(x) || x < 1 || x > max) {
    stop("'", nom, "' must be a single integer",
         if (is.finite(max)) paste0(" between 1 and ", max) else " >= 1",
         call. = FALSE)
  }
}
