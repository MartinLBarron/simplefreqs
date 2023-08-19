
# Testing options ---------------------------------------------------------


test_that("Works with inner_table_padding", {
  
  options(simplefreqs.inner_table_padding = 2)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.inner_table_padding = NULL)
})

test_that("Works with table_symbol", {
  options(simplefreqs.table_symbol = "#")
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.table_symbol = NULL)
})

test_that("Works with row_divider_symbol", {
  options(simplefreqs.row_divider_symbol = "#")
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.row_divider_symbol = NULL)
})

test_that("Works with print_table_symbol", {
  options(simplefreqs.print_table_symbol = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.print_table_symbol = NULL)
})

test_that("Works with print_table_total_row", {
  options(simplefreqs.print_table_total_row = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.print_table_total_row = NULL)
})

test_that("Works with print_table_metadata", {
  options(simplefreqs.print_table_metadata = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.print_table_metadata = NULL)
})

test_that("Works with print_header_divider", {
  options(simplefreqs.print_header_divider = F)
  expect_snapshot(freq(iris, Species, plot = F, markdown=F))
  options(simplefreqs.print_header_divider = NULL)
})

