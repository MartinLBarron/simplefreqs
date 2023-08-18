# Order of values when not sorted  --------------------------------------------

test_that("order when fed a raw character is alphabetic", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D")
  df <- data.frame(x = x)
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})

test_that("order when fed a raw character is alphabetic, with NA at end", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = x)
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})

test_that("order when fed a unspecified factor is alphabetic, with NA at end", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = factor(x))
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})

test_that("if factor has other ordering freq will use that", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = factor(x, levels = c("D", "B", "C", "A")))
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})

test_that("an ordered variable is also retained", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = factor(x, levels = c("D", "C", "B", "A")))
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})

test_that("an ordered variable is overrode by sort", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = factor(x, levels = c("D", "C", "B", "A")))
  expect_snapshot(freq(df, x, sort = T, plot = F, markdown=F))
})

test_that("a numeric is also sorted", {
  x <- c(1, 1, 1, 2, 2, 2, 2, 10, NA)
  df <- data.frame(x = factor(x))
  expect_snapshot(freq(df, x, sort = F, plot = F, markdown=F))
})
