
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
