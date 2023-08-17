# Weighted ------------------------------------------------------------

test_that("basic weighted results", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "A", "C", "C", "C", "C", "D", "D", "D", "D", NA)
  wt <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5)
  df <- data.frame(x = x, wt = wt)
  expect_snapshot(freq(df, x, wt = wt, plot = F))
})
