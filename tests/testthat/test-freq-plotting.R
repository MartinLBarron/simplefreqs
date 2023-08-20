
# Plotting ------------------------------------------------------------

test_that("basic plotting1", {
  library(ggplot2)
  
  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    print(freq(iris, Species, markdown=F, plot = T))
    on.exit(dev.off())
    
    path
  }
  
    skip_on_cran()
    expect_snapshot_file(save_png(), "plot1.png")
  
})


