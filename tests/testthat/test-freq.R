test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("basic freq of vector", {
  expect_snapshot(freq(iris$Species, plot = F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F))
})
