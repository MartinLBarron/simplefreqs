
# Plotting ------------------------------------------------------------

test_that("basic html output", {
  library(ggplot2)
  
  save_rmd_html <- function() {
    path1 <- tempfile(fileext = ".html")
    path2 <- tempfile(fileext = ".png")
    webshot::install_phantomjs()
    rmarkdown::render(input="test1.Rmd", output_file=path1, output_format = "html_document")
    webshot::webshot(path1, path2)
    path2
  }
  
  skip_on_cran()
    expect_snapshot_file(save_rmd_html(), "rmd_html.png")
})
