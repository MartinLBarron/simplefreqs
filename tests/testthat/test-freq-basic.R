
# core formats ------------------------------------------------------------

test_that("basic freq of vector", {
  expect_snapshot(freq(iris$Species, plot = F, markdown=F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
})

test_that("basic freq of indexing", {
  expect_snapshot(freq(iris[["Species"]], plot = F, markdown=F))
})

## Other things to test
# na.rm = T
# sorting
# plotting results
# as part of dplyr chain
# as part of base chain


