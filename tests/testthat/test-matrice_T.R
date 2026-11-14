test_that("matrice_T builds the right matrix", {
  expect_equal(matrice_T(3, 1, 3),
               matrix(c(0, 0, 1, 0, 1, 0, 1, 0, 0), 3))
  expect_equal(matrice_T(3, 1, 2), matrice_T(3, 2, 1))
})

test_that("matrice_T is its own inverse", {
  Tm <- matrice_T(4, 1, 3)
  expect_equal(Tm %*% Tm, diag(4))
})

test_that("matrice_T rejects i = j and invalid indices", {
  expect_error(matrice_T(3, 2, 2), "different")
  expect_error(matrice_T(3, 4, 1))
  expect_error(matrice_T(3, 1.5, 2))
  expect_error(matrice_T(3, c(1, 2), 3))
})

test_that("echange works on non-square matrices", {
  A <- matrix(1:6, nrow = 3)
  expect_equal(echange(A, 1, 3), A[c(3, 2, 1), ])
  expect_equal(echange(A, 1, 2, "columns"), A[, c(2, 1)])
})

test_that("echange rejects an invalid side", {
  expect_error(echange(matrix(1:4, 2), 1, 2, "diagonal"))
})


