
# Different classes of vectors --------------------------------------------

test_that("Works with factors", {
  expect_snapshot(freq(iris, Species, plot = F))
})

test_that("Works with Character", {
  expect_snapshot(freq(as.character(iris$Species), plot = F))
})


test_that("Works with character2", {
  x <- c("A","A","B","C","D","D","D",NA)
  df <- data.frame(x=x)
  expect_snapshot(freq(df, x, plot = F))
})


test_that("Works with Numeric", {
  expect_snapshot(freq(iris, Sepal.Width, plot = F))
})

test_that("Works with numeric2", {
  x <- c(1,2,3,3,4,2,1,3,2)
  df <- data.frame(x=x)
  expect_snapshot(freq(df, x, plot = F))
})

test_that("Works with Dates", {
  df <- data.frame(AirPassengers, year = trunc(time(AirPassengers)), 
                   month = month.abb[cycle(AirPassengers)])
  df <- df %>%
    mutate(dt=as.Date(paste0(year,"-", month,"-", 1), format="%Y-%b-%d"))
  
  expect_snapshot(freq(df$dt, plot = F))
})

test_that("Works with dates2", {
  
  x <- c(as.Date("2021-01-01"),as.Date("2021-01-01"),as.Date("2022-01-01"),as.Date("2022-01-01"),
         as.Date("2023-01-01"),as.Date("2023-01-01"),as.Date("2023-01-01"),as.Date("2021-01-01"))
  df <- data.frame(x=x)
  expect_snapshot(freq(df, x, plot = F))
})

test_that("Works with Times", {
  time <- as.POSIXct(c(1,2,3,3,3,4))
  df <- data.frame(time=time)
  expect_snapshot(freq(df$time, plot = F))
})

test_that("Works with POSIXct", {
  x <- c(as.POSIXct("2021-01-01"),as.POSIXct("2021-01-01"),as.POSIXct("2022-01-01"),as.POSIXct("2022-01-01"),
         as.POSIXct("2023-01-01"),as.POSIXct("2023-01-01"),as.POSIXct("2023-01-01"),as.POSIXct("2021-01-01"))
  df <- data.frame(x=x)
  expect_snapshot(freq(df, x, plot = F))
})


test_that("Works with logical", {
  x <- c(T,T,F,F,T,T,F,F,F,F)
  expect_snapshot(freq(x, plot = F))
})

test_that("Works with complex", {
  x <- c(1i,2i,3i,3i,4i,2i,1i,3i,2i)
  df <- data.frame(x=x)
  expect_snapshot(freq(df, x, plot = F))
})







