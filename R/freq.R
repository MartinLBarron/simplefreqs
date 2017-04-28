#Required packages
#devtools::install_github("tidyverse/dplyr")
library(dplyr)

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
#' @param use.NA if TRUE (default) NAs are included in frequency list.  If FALSE, NA are removed (but reported seperately)
#' @return The original dataframe or table containing frequencies, Produces side-effect of printed frequencie table
#' @examples
#' freq(iris, Species)
#'
#'
#' @import dplyr
#' @export freq
#' 


freq <- function(df, x, saveResults=F, printResults=T, sortResults=T, levelWarning=T, use.NA=T){
  
  #Capture input variable for non-standard evaluation
  enquo_x <- enquo(x)
  
  #Save df as is for later return
  df_orig=df
  
  #remove NA if use.NA=F
  if (use.NA==F){
    naCount <- df %>%
      filter(is.na(UQE(enquo_x)))
    df=filter(df, !is.na(UQE(enquo_x)))
  }
  
  #check number of levels and give appropriate errors
  if (levelWarning==T){
    l <- length(levels(factor(df[[quo_name(enquo_x)]])))
    print(l)
    if (l>25){
      stop(paste(quo_name(enquo_x),"contains more than 25 levels (has", length(levels(factor(df[[quo_name(enquo_x)]]))), "levels). Set warning=FALSE to proceed."))
    }
  }
  
  #Calculate frequencies
  df<-  df %>%
    count(factor(UQE(enquo_x)), sort=sortResults) %>%
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
  n <- names(dfprint)
  n[1] <- quo_name(enquo_x)
  names(dfprint) <- n
  
  #Print results as requested
  if (printResults==T){
    dfprint <- as.data.frame(dfprint)
    print(dfprint, justify="left", row.names=F)
    
    if (use.NA==F){
      naCount1 <- nrow(naCount)
      naPercent<-(naCount1/nrow(df_orig))*100
      cat("______________________\n")
    cat(paste0("NA excluded (", prettyNum(naCount1, big.mark=","), ", ", formatC(naPercent, digits=1, format="f"), "%)"))
    }
  }
  #Save results as requested
  if (saveResults==T){
    return(df)
  }
  else{
    return(df_orig)
  }
}
