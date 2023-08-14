
# core formats ------------------------------------------------------------


test_that("basic freq of vector", {
  expect_snapshot(freq(iris$Species, plot = F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("basic freq of tidy vector", {
  expect_snapshot(freq(iris, Species, plot = F))
})

# Other formats that SHOULD NOT work

# Other formats -----------------------------------------------------------


test_that("basic freq of List", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("basic freq of matrix", {
  expect_snapshot(freq(iris, Species, plot = F))
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

#TODO: add this
test_that("Works with Times", {
  expect_snapshot(freq(iris, Species, plot = F))
})


