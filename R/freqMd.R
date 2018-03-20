#' freqMd
#' 
#' @title Caclulate frequencies of variable and formats for printing in markdown
#'
#' @description
#' \code{freqMd} prints a frequency table for each variable passed to it.
#' 
#'
#' @param ... A dataframe

#' @import dplyr
#' @import ggplot2
#' @import rlang
#' 
#' @export
#' 

freqMd <- function(...){
  freq(..., plotResults = F, printResults = F, saveResults = T)
}