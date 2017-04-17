#Required packages
#devtools::install_github("tidyverse/dplyr")
#' @title Caclulate frequencies of variable
#'
#' @description
#' \code{freq} prints a frequency table for each variable passed to it.
#' 
#' @details
#' This function prints a frequency table for variables passed to it.  In
#' order to fit within the tidyverse, it takes as its first argument a dataframe
#' and returns that (unaltered) dataframe, unless the user explicilty requests other output
#'
#' @param df A dataframe
#' @param x a variable in associated dataframe.
#' @param saveResults If FALSE (default) returns original dataframe. If TRUE, returns frequency results as a dataframe. This allows user to place this function inside a dplyr chain
#' @param print_results If TRUE (default), prints results to console.  Otherwise, if FALSE, no results are printed.
#' @param sortResults If TRUE (default), sort output in descending order of n. If FALSE, sort output in ascending order of levels
#' @param levelWarning if TRUE (default) gives an error if the variable passed has more than 25 levels. If
#'
#' @return The original dataframe or table containing frequencies, Produces side-effect of printed frequencie table
#' @examples
#' mbfreq(iris, Species)
#'
#'
#' @import dplyr
#' @export freq
#' 

library(dplyr)

freq <- function(df, x, saveResults=F, printResults=T, sortResults=T, levelWarning=T, M){
  
  #Capture input variable for non-standard evaluation
  x <- enquo(x)
  
  #Save df as is for later return
  df_orig=df
  
  #check number of levels and give appropriate errors
  if (levelWarning==T){
    if (length(levels(factor(df[[quo_name(x)]])))>25){
      stop(paste(quo_name(x),"contains more than 25 levels (has", length(levels(factor(df[[quo_name(x)]]))), "levels). Set warning=FALSE to proceed."))
    }
  }
  
  #Calculate frequencies
  df<-  df %>%
    count(factor(UQ(x)), sort=sortResults) %>%
    mutate(percentage = n/sum(n),
           cumulative = cumsum(n),
           cumulative_percent = cumulative/sum(n)
    )
  
  #Format for printing
  dfprint <-  df %>%
    mutate(n = formatC(n, format="f", digits=0, big.mark=","),
           percentage = paste0(formatC(100 * percentage, digits=1, format="f"), "%"),
           cumulative = formatC(cumulative, format="f", digits=0, big.mark = ","),
           cumulative_percent = paste0(formatC(100 * cumulative_percent, digits=1, format="f"), "%")
    )
  
  #Print results as requested
  if (printResults==T){
    dfprint <- as.data.frame(dfprint)
    print(dfprint, justify="left")
  }
  #Save results as requested
  if (saveResults==T){
    return(df)
  }
  else{
    return(df_orig)
  }
}


##Testing
# 
# # testing -----------------------------------------------------------------
# 
# library(forcats)
# dfgss <-gss_cat
# 
# 
# # Simple ------------------------------------------------------------------
# 
# df <-freq(dfgss, year)
# 
# 
# 
# # save results ------------------------------------------------------------
# df <- freq(dfgss, year, saveResults = T)
# 
# # sort --------------------------------------------------------------------
# df <- freq(dfgss, year, sortResults = F)
# 
# 
# # print Results -----------------------------------------------------------
# 
# df <- freq(dfgss, year, printResults = F)
# 
# 
# # Level warnings ----------------------------------------------------------
# df <- freq(dfgss, denom)
# df <- freq(dfgss, denom, levelWarning = F)
# 
# 
# # Missings ----------------------------------------------------------------
# 
# dfgss$year[c(seq(1,1000, by=4))] <- NA
# 
# x <-freq(dfgss, year, saveResults=T, sortResults = T)
# 
# 
# # As part of a dplyr chain ------------------------------------------------
# 
# 
# #As part of chain
# df <- dfgss %>%
#   filter(year>2006) %>%
#   freq(year)


