#' Rank zones by priority using transposition matrices
#'
#' Computes a priority score for each zone from the frequency, the duration
#' and the severity of power cuts, then sorts the zones from the most to the
#' least affected. The sort is a sequence of row swaps, each one a product
#' by a matrix T_ij (see \code{\link{echange}}).
#'
#' @param data A data frame with the columns \code{quartier},
#'   \code{frequence}, \code{duree} and \code{gravite}.
#' @param poids Named numeric vector of weights for \code{frequence},
#'   \code{duree} and \code{gravite}. Each variable is divided by its
#'   maximum, then the weighted mean is computed.
#' @return A data frame sorted by decreasing score, with the columns
#'   \code{rang}, \code{quartier}, \code{score}, \code{frequence},
#'   \code{duree} and \code{gravite}. The swaps used are stored in the
#'   attribute \code{"echanges"} (a list of pairs of row indices).
#' @examples
#' d <- data.frame(quartier = c("A", "B", "C"),
#'                 frequence = c(4, 12, 8),
#'                 duree = c(2, 3, 1),
#'                 gravite = c(2, 5, 3))
#' classer_zones(d)
#' @export
classer_zones <- function(data,
                          poids = c(frequence = 1, duree = 1, gravite = 1)) {
  vars <- c("frequence", "duree", "gravite")
  if (!is.data.frame(data) || !all(c("quartier", vars) %in% names(data))) {
    stop("'data' must be a data frame with columns: quartier, ",
         "frequence, duree, gravite", call. = FALSE)
  }
  num <- as.matrix(data[, vars])
  if (!is.numeric(num) || anyNA(num) || any(num < 0)) {
    stop("frequence, duree and gravite must be non-negative numbers ",
         "without missing values", call. = FALSE)
  }
  if (!all(vars %in% names(poids)) || any(poids[vars] < 0) ||
      sum(poids[vars]) == 0) {
    stop("'poids' must be a named vector of non-negative weights",
         call. = FALSE)
  }
  w <- poids[vars] / sum(poids[vars])
  mx <- apply(num, 2, max)
  mx[mx == 0] <- 1
  score <- drop(sweep(num, 2, mx, "/") %*% w)

  M <- cbind(score = score, num)
  rownames(M) <- as.character(data$quartier)
  n <- nrow(M)
  paires <- list()
  for (k in seq_len(n - 1)) {
    pos <- k - 1 + which.max(M[k:n, "score"])
    if (pos != k) {
      M <- echange(M, k, pos)
      paires[[length(paires) + 1]] <- c(k, pos)
    }
  }
  res <- data.frame(rang = seq_len(n), quartier = rownames(M),
                    score = round(M[, "score"], 3),
                    M[, vars, drop = FALSE], row.names = NULL)
  attr(res, "echanges") <- paires
  res
}
