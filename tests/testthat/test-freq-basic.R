
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
