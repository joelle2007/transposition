test_that("transposer gives the transpose", {
  A <- matrix(1:6, nrow = 2)
  expect_equal(transposer(A), t(A))
  expect_equal(dim(transposer(A)), c(3, 2))
  B <- matrix(1:12, nrow = 4)
  expect_equal(transposer(B), t(B))
  C <- matrix(1:9, nrow = 3)
  expect_equal(transposer(C), t(C))
})

test_that("transposer handles a single row and a single entry", {
  r <- matrix(1:5, nrow = 1)
  expect_equal(transposer(r), t(r))
  expect_equal(transposer(matrix(7)), matrix(7))
})

test_that("transposing twice gives back A and both methods agree", {
  A <- matrix(1:6, nrow = 2)
  expect_equal(transposer(transposer(A)), A)
  expect_equal(transposer(A, "direct"), transposer(A))
})

test_that("the swaps method refuses large matrices", {
  expect_error(transposer(matrix(0, 30, 30)), "direct")
  expect_equal(dim(transposer(matrix(0, 30, 40), "direct")), c(40, 30))
})





