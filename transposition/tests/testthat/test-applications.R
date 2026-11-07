test_that("echange_elements swaps two elements of a vector", {
  expect_equal(echange_elements(c(1, 2, 3), 2, 3), c(1, 3, 2))
  expect_equal(echange_elements(c(a = 1, b = 2, c = 3), 2, 3),
               c(a = 1, c = 3, b = 2))
  expect_error(echange_elements(c(1, 2, 3), 2, 2), "different")
  expect_error(echange_elements("a", 1, 2))
  expect_error(echange_elements(matrix(1:4, 2), 1, 2))
})

test_that("echange_variables swaps two columns of a data frame", {
  d <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  expect_equal(names(echange_variables(d, "b", "c")), c("a", "c", "b"))
  expect_equal(echange_variables(d, 2, 3), d[, c(1, 3, 2)])
  expect_error(echange_variables(d, "zz", "a"), "unknown")
  expect_error(echange_variables(d, "a", "a"), "different")
  expect_error(echange_variables(1:3, 1, 2), "data frame")
})



