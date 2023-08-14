
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


# Other formats  that should not work ------------------------------------


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


# Different classes of vectors --------------------------------------------

test_that("Works with factors", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("Works with Character", {
  expect_snapshot(freq(as.character(iris$Species), plot = F))
})

test_that("Works with Numeric", {
  expect_snapshot(freq(iris, Sepal.Width, plot = F))
})

test_that("Works with Dates", {
  df <- data.frame(AirPassengers, year = trunc(time(AirPassengers)), 
                   month = month.abb[cycle(AirPassengers)])
  df <- df %>%
    mutate(dt=as.Date(paste0(year,"-", month,"-", 1), format="%Y-%b-%d"))
  
  expect_snapshot(freq(df$dt, plot = F))
})


test_that("Works with Times", {
  time <- as.POSIXct(c(1,2,3,3,3,4))
  df <- data.frame(time=time)
  expect_snapshot(freq(df$time, plot = F))
})


# Test plots --------------------------------------------------------------

test_that("Works with plots", {
  tester <- function(){
    freq(df$dt, plot = )
  }
  expect_snapshot_file(tester)
})


