#' Swap two elements of a vector
#'
#' Computes T_ij \%*\% x, which swaps elements i and j of a numeric vector.
#'
#' @param x A numeric vector.
#' @param i,j Distinct positions in x.
#' @return A numeric vector with elements i and j swapped.
#' @examples
#' echange_elements(c(1, 2, 3), 2, 3)
#' @export
echange_elements <- function(x, i, j) {
  if (!is.numeric(x) || !is.null(dim(x))) {
    stop("'x' must be a numeric vector", call. = FALSE)
  }
  res <- drop(matrice_T(length(x), i, j) %*% x)
  nm <- names(x)
  if (!is.null(nm)) {
    nm[c(i, j)] <- nm[c(j, i)]
    names(res) <- nm
  }
  res
}

#' Swap two variables (columns) of a data frame
#'
#' Reorders the columns of a data frame so that two variables exchange
#' their positions. Other columns, names and types are kept.
#'
#' @param data A data frame.
#' @param var1,var2 Names or positions of the two variables.
#' @return The data frame with the two variables swapped.
#' @examples
#' d <- data.frame(Temperature = 1:2, Rain = 3:4, Humidity = 5:6)
#' echange_variables(d, "Rain", "Humidity")
#' @export
echange_variables <- function(data, var1, var2) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame", call. = FALSE)
  }
  pos <- function(v) if (is.character(v)) match(v, names(data)) else v
  i <- pos(var1)
  j <- pos(var2)
  if (anyNA(c(i, j))) {
    stop("unknown variable name", call. = FALSE)
  }
  ordre <- echange_elements(seq_len(ncol(data)), i, j)
  data[, ordre, drop = FALSE]
}

#' Swap two indices of a square matrix
#'
#' Renumbers two indices i and j: rows and columns are swapped at the same
#' time, which is T_ij A T_ij. For an adjacency matrix, the network is the
#' same, only two nodes have exchanged their numbers.
#'
#' @param A A square matrix.
#' @param i,j Distinct indices to exchange.
#' @return The matrix T_ij A T_ij.
#' @examples
#' A <- matrix(0, 4, 4)
#' A[cbind(c(1, 2, 3), c(2, 3, 4))] <- 1
#' A <- A + t(A)
#' echange_indices(A, 1, 3)
#' @export
echange_indices <- function(A, i, j) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) {
    stop("'A' must be a square matrix", call. = FALSE)
  }
  Tm <- matrice_T(nrow(A), i, j)
  Tm %*% A %*% Tm
}

