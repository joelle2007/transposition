test_that("graphics functions run and return the right matrix", {
  grDevices::pdf(NULL)
  A <- matrix(1:6, nrow = 3)
  expect_equal(visualiser_echange(A, 1, 3), A[c(3, 2, 1), ])
  expect_equal(visualiser_matrice(A), A)
  expect_error(visualiser_matrice("a"))
  grDevices::dev.off()
})
