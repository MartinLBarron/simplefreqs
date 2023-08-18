
# core formats ------------------------------------------------------------

test_that("NA Remove works", {
  x <- c("B", "B", "B", "B", "A", "A", "A", "C", "C", "D", NA)
  df <- data.frame(x = factor(x, levels = c("D", "C", "B", "A")))
  expect_snapshot(freq(df, x, na.rm=T, plot = F, markdown=F))
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

