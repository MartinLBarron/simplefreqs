
# Formats that should not work ------------------------------------


test_that("basic freq of List", {
  list_data <- list("Red", "Green", c(21,32,11), TRUE, 51.23, 119.1)
  expect_error(freq(list_data))
})

test_that("basic freq of matrix", {
  matrix_data <- matrix(c(1,2,3,4), nrow=2)
  expect_error(freq(matrix_data))
})

test_that("basic freq of array", {
  array_data <- array(c(1:24), dim=c(4,3,2))
  expect_error(freq(array_data))
})

