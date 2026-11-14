d <- data.frame(quartier = c("A", "B", "C"), frequence = c(4, 12, 8),
                duree = c(2, 3, 1), gravite = c(2, 5, 3))

test_that("classer_zones sorts zones by decreasing priority", {
  r <- classer_zones(d)
  expect_equal(r$quartier, c("B", "C", "A"))
  expect_equal(r$rang, 1:3)
  expect_equal(unname(r$score), c(1, 0.533, 0.467))
})f

test_that("the swaps used are stored and replay the sort", {
  r <- classer_zones(d)
  p <- attr(r, "echanges")
  expect_true(all(lengths(p) == 2))
  ordre <- 1:3
  for (pr in p) ordre[pr] <- ordre[rev(pr)]
  expect_equal(d$quartier[ordre], r$quartier)
})

test_that("weights change the ranking", {
  r <- classer_zones(d, c(frequence = 0, duree = 1, gravite = 0))
  expect_equal(r$quartier, c("B", "A", "C"))
})

test_that("zone names are kept after many swaps", {
  big <- data.frame(quartier = letters[1:6], frequence = c(1, 6, 3, 5, 2, 4),
                    duree = rep(1, 6), gravite = rep(1, 6))
  expect_equal(classer_zones(big)$quartier, c("b", "d", "f", "c", "e", "a"))
})

test_that("classer_zones rejects bad input", {
  expect_error(classer_zones(1:3), "data frame")
  expect_error(classer_zones(d[, c("quartier", "duree")]), "columns")
  bad <- d
  bad$duree[1] <- NA
  expect_error(classer_zones(bad), "non-negative")
  expect_error(classer_zones(d, c(frequence = 1, duree = 1)), "poids")
})
