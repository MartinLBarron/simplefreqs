
# Testing options ---------------------------------------------------------


test_that("Works with inner_table_padding", {
  
  options(SimpleFreqs.inner_table_padding = 2)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.inner_table_padding = NULL)
})

test_that("Works with table_symbol", {
  options(SimpleFreqs.table_symbol = "#")
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.table_symbol = NULL)
})

test_that("Works with row_divider_symbol", {
  options(SimpleFreqs.row_divider_symbol = "#")
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.row_divider_symbol = NULL)
})

test_that("Works with print_table_symbol", {
  options(SimpleFreqs.print_table_symbol = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.print_table_symbol = NULL)
})

test_that("Works with print_table_total_row", {
  options(SimpleFreqs.print_table_total_row = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.print_table_total_row = NULL)
})

test_that("Works with print_table_metadata", {
  options(SimpleFreqs.print_table_metadata = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.print_table_metadata = NULL)
})

test_that("Works with print_header_divider", {
  options(SimpleFreqs.print_header_divider = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(SimpleFreqs.print_header_divider = NULL)
})

