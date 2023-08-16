
# core formats ------------------------------------------------------------

test_that("basic freq of vector", {
  expect_snapshot(freq(iris$Species, plot = F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("basic freq of indexing", {
  expect_snapshot(freq(iris[["Species"]], plot = F))
})

## Other things to test
# na.rm = T
# sorting
# plotting results
# as part of dplyr chain
# as part of base chain
# what is order if unordered
# what is order if an ordered variable
# what is order for a factor

