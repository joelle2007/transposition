test_that("est_transposition recognises T_ij", {
  expect_true(est_transposition(matrice_T(4, 1, 3)))
  expect_false(est_transposition(diag(3)))
  expect_false(est_transposition(matrix(1:4, 2)))
  expect_false(est_transposition(matrix(1:6, 2)))
  expect_false(est_transposition("a"))
  expect_false(est_transposition(diag(3)[c(2, 3, 1), ]))
})

test_that("proprietes_T gives the right properties", {
  p <- proprietes_T(4, 1, 3)
  expect_true(p$symetrique)
  expect_true(p$orthogonale)
  expect_true(p$involutive)
  expect_equal(p$determinant, -1)
  expect_error(proprietes_T(4, 2, 2), "different")
})

test_that("echanges applies swaps in order", {
  A <- matrix(1:6, nrow = 3)
  expect_equal(echanges(A, list(c(1, 3), c(2, 3))), A[c(3, 1, 2), ])
  expect_equal(echanges(A, list(c(1, 2)), "columns"), A[, c(2, 1)])
  expect_equal(echanges(A, list()), A)
})

test_that("echanges rejects bad input", {
  expect_error(echanges(diag(3), c(1, 2)))
  expect_error(echanges(diag(3), list(c(1, 2, 3))))
  expect_error(echanges(diag(3), list(c(1, 1))))
})

