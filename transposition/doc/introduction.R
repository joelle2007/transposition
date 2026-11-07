## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(transposition)

## -----------------------------------------------------------------------------
matrice_T(4, 1, 3)

## -----------------------------------------------------------------------------
A <- matrix(1:6, nrow = 2)
A
echange(A, 1, 3, "columns")

## -----------------------------------------------------------------------------
proprietes_T(4, 1, 3)

## -----------------------------------------------------------------------------
m <- 8
n <- m * m
K <- matrix(0, n, n)
for (r in 1:m) {
  for (c in 1:m) {
    k <- (r - 1) * m + c
    K[k, k] <- 4
    if (c < m) { K[k, k + 1] <- -1; K[k + 1, k] <- -1 }
    if (r < m) { K[k, k + m] <- -1; K[k + m, k] <- -1 }
  }
}
f <- seq_len(n) / n

bandwidth <- function(M) {
  idx <- which(M != 0, arr.ind = TRUE)
  max(abs(idx[, 1] - idx[, 2]))
}
spy <- function(M, title) {
  graphics::image(seq_len(ncol(M)), seq_len(nrow(M)),
                  t(M != 0)[, nrow(M):1] * 1,
                  col = c("red", "steelblue"), axes = FALSE,
                  xlab = "", ylab = "", main = title)
  graphics::box()
}

bandwidth(K)

## -----------------------------------------------------------------------------
set.seed(1)
p <- sample(n)
K_bad <- K[p, p]
f_bad <- f[p]
bandwidth(K_bad)

## -----------------------------------------------------------------------------
u <- solve(K, f)
u_bad <- solve(K_bad, f_bad)
all.equal(u_bad, u[p])

## -----------------------------------------------------------------------------
K_new <- K_bad
f_new <- f_bad
current <- p
n_swaps <- 0
for (k in seq_len(n)) {
  pos <- which(current == k)
  if (pos != k) {
    Tm <- matrice_T(n, k, pos)
    K_new <- Tm %*% K_new %*% Tm
    f_new <- drop(Tm %*% f_new)
    current[c(k, pos)] <- current[c(pos, k)]
    n_swaps <- n_swaps + 1
  }
}
n_swaps
bandwidth(K_new)

## -----------------------------------------------------------------------------
all.equal(K_new, K)
all.equal(drop(solve(K_new, f_new)), u)

## ----fig.width = 9, fig.height = 3.5------------------------------------------
old <- graphics::par(mfrow = c(1, 3))
spy(K, "Natural numbering")
spy(K_bad, "Random numbering")
spy(K_new, "After the swaps")
graphics::par(old)

