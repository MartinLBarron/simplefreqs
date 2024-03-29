# simplefreqs

`simplefreqs` is a package for making simple (and attractive) frequency 
tables. It outputs nicely to the console, to html, to pdf, and to word.

## Installation

```
# Install simplefreqs from CRAN (coming soon...)
# install.packages("simplefreqs")

# install from GitHub using remotes:
# install.packages("remotes")
remotes::install_github("MartinLBarron/simplefreqs", build_vignettes = TRUE)

# install the development version from GitHub using remotes:
# install.packages("remotes")
remotes::install_github("MartinLBarron/simplefreqs@dev", build_vignettes = TRUE)
```

## Usage

The `simplefreqs` package contains a single function, `freq`.  At its most basic,
`freq` accepts a data.frame and variable as inputs and calculates the resulting
frequency table.  This can be done as a stand-alone operation or as part of a pipeline.

```
# Stand-alone
freq(iris, Species)

# As part of pipeline
iris |>
  freq(Species)

```
See basic usage vignette (`vignette('basic-use', 'simplefreqs')` for more details.
