#' Plot a matrix as a coloured grid
#'
#' Draws each entry of a numeric matrix as a coloured cell with its value
#' written inside. Row 1 is at the top, as in the usual matrix notation.
#'
#' @param M A numeric matrix.
#' @param titre Title of the plot (character).
#' @return The matrix \code{M}, invisibly.
#' @examples
#' visualiser_matrice(matrice_T(4, 1, 3), "T_13")
#' @export
visualiser_matrice <- function(M, titre = "") {
  M <- as.matrix(M)
  if (!is.numeric(M)) stop("'M' must be a numeric matrix", call. = FALSE)
  nr <- nrow(M)
  nc <- ncol(M)
  graphics::image(
    x = seq_len(nc), y = seq_len(nr),
    z = t(M)[, nr:1, drop = FALSE],
    col = grDevices::colorRampPalette(c("white", "#9ecae1"))(12),
    axes = FALSE, xlab = "Columns", ylab = "Rows", main = titre
  )
  graphics::axis(1, at = seq_len(nc), labels = seq_len(nc))
  graphics::axis(2, at = seq_len(nr), labels = rev(seq_len(nr)), las = 1)
  graphics::box()
  g <- expand.grid(r = seq_len(nr), c = seq_len(nc))
  graphics::text(g$c, nr - g$r + 1, labels = M[cbind(g$r, g$c)])
  invisible(M)
}

#' Show a matrix before and after swapping two rows or columns
#'
#' Plots the matrix \code{A} and the matrix obtained with
#' \code{\link{echange}}, side by side.
#'
#' @inheritParams echange
#' @return The matrix after the swap, invisibly.
#' @examples
#' A <- matrix(1:6, nrow = 3)
#' visualiser_echange(A, 1, 3)
#' visualiser_echange(A, 1, 2, "columns")
#' @export
visualiser_echange <- function(A, i, j, cote = c("rows", "columns")) {
  cote <- match.arg(cote)
  A <- as.matrix(A)
  B <- echange(A, i, j, cote)
  old <- graphics::par(mfrow = c(1, 2))
  on.exit(graphics::par(old))
  visualiser_matrice(A, "Before")
  visualiser_matrice(B, paste0("After (", cote, " ", i, " and ", j, ")"))
  invisible(B)
}


#' Plot the priority ranking of zones
#'
#' Draws a horizontal bar chart of the priority scores, the most affected
#' zone at the top.
#'
#' @param classement A data frame returned by \code{\link{classer_zones}}.
#' @param titre Title of the plot.
#' @return The data frame \code{classement}, invisibly.
#' @examples
#' d <- data.frame(quartier = c("A", "B", "C"), frequence = c(4, 12, 8),
#'                 duree = c(2, 3, 1), gravite = c(2, 5, 3))
#' visualiser_classement(classer_zones(d))
#' @export
visualiser_classement <- function(classement,
                                  titre = "Priority ranking of zones") {
  if (!is.data.frame(classement) ||
      !all(c("quartier", "score") %in% names(classement))) {
    stop("'classement' must come from classer_zones()", call. = FALSE)
  }
  n <- nrow(classement)
  cols <- grDevices::colorRampPalette(c("blue", "red"))(n)
  old <- graphics::par(mar = c(4, 9, 3, 1))
  on.exit(graphics::par(old))
  graphics::barplot(rev(classement$score), names.arg = rev(classement$quartier),
                    horiz = TRUE, las = 1, col = rev(cols), xlim = c(0, 1),
                    xlab = "Priority score (0 to 1)", main = titre)
  invisible(classement)
}
