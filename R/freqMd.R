#' freqMd
#' 
#' @title Caclulate frequencies of variable and formats for printing in markdown
#'
#' @description
#' \code{freqMd} prints a frequency table for each variable passed to it.
#' 
#'
#' @param df A dataframe
#' @param x a variable in associated dataframe.
#' @param saveResults If FALSE (default) returns nothing. If TRUE (default), returns frequency results as a dataframe. This allows user to place this function inside a dplyr chain
#' @param plotResults if TRUE (default) prints bar chart of results.  If FALSE, no chart.
#' @param printResults If TRUE (default), prints results to console.  Otherwise, if FALSE, no results are printed.
#' @param sortResults If TRUE (default), sort output in descending order of n. If FALSE, sort output in ascending order of levels
#' @param levelError if TRUE (default) gives an error if the variable passed has more than 100 levels. If
#' @param na.rm if TRUE (default) NAs are included in frequency list.  If FALSE, NA are removed (but reported seperately)
#' @return Null or dataframe containing frequencies. Prints frequency tabl
#' @examples
#' \dontrun{
#' freq(iris, Species)
#'}
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' 
#' @export
#' 

freqMd <- function(df, ..., plotResults=T, saveResults=F, printResults=T, sortResults=T, levelError=T, na.rm=F){
  f
}

