
# core formats ------------------------------------------------------------

test_that("works if padding set small and total row is large", {
  library(dplyr)
  options(SimpleFreqs.inner_table_padding = 1)
  data("storms")
  expect_snapshot(freq(storms, day, plot = F, markdown=F))
  options(SimpleFreqs.inner_table_padding = NULL)
})
