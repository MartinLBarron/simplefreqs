
# Plotting ------------------------------------------------------------

test_that("basic plotting", {
  library(ggplot2)
  save_png <- function() {
    freq(iris, Species)
    path <- tempfile(fileext = ".png")
    ggsave(path)
    print(path)
    path
  }

  expect_snapshot_file(save_png(), "plot.png")
})


