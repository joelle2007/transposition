output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
```

# transposition

The goal of `transposition` is to build the transposition matrix
T_ij = I_n - E_ii - E_jj + E_ij + E_ji, which is the identity matrix with
rows i and j swapped (i != j).

Multiplying a matrix A on the left by T_ij swaps rows i and j of A.
Multiplying on the right swaps columns i and j. Both work for square and
non-square matrices.

## Installation

From the package folder:

```r
devtools::install()
```

## Example

```{r example}
library(transposition)

# The transposition matrix T_13 of size 4
matrice_T(4, 1, 3)

# Swap rows 1 and 3 of a non-square matrix
A <- matrix(1:6, nrow = 3)
echange(A, 1, 3)

# Swap columns 1 and 2
echange(A, 1, 2, "columns")
```

## Properties

```{r properties}
est_transposition(matrice_T(4, 1, 3))
proprietes_T(4, 1, 3)
```

## Functions

| Function | Description |
|---|---|
| `matrice_T()` | Builds the transposition matrix T_ij |
| `echange()` | Swaps two rows or two columns of a matrix |
| `echanges()` | Applies several swaps in a row |
| `est_transposition()` | Tests whether a matrix is a transposition matrix |
| `proprietes_T()` | Checks the properties of T_ij |

## License

MIT


