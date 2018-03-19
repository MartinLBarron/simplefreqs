#' freq
#' 
#' @title Caclulate frequencies of variable
#'
#' @description
#' \code{freq} prints a frequency table for each variable passed to it.
#' 
#' @details
#' This function prints a frequency table for variables passed to it.  In
#' order to fit within the tidyverse, it takes as its first argument a dataframe
#' and returns the frequency table as a dataframe. See ]code{freqMd} for version
#' appropriate for easy markdown printing.
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

freq <- function(df, ..., plotResults=T, saveResults=F, printResults=T, sortResults=T, levelError=T, na.rm=F){
  #library(rlang)

  #Save df as is for later return
  df_orig=df
  
  ## Deal with ...
  #Capture all variables passed in
  vars <- quos(...)

  #if no vars passed in, get all variables in dataframe
  #this requires special "sym" command from rlang
  if (length(vars)==0){
    vars <- list()
    nme <- names(df)
    for (i in 1:length(nme)){
      x <- nme[[i]]
      gp <- quo(!! rlang::sym(x))
      vars[[i]]<-unlist(gp)
    }
  }
  
  for (var in vars){
    df <- df_orig
    levelErrorNumber <- 100
    #Capture input variable for non-standard evaluation
    enquo_x <- var
    
    #remove NA if na.rm=T
    if (na.rm==T){
      naCount <- df %>%
        filter(is.na(UQE(enquo_x)))
      df=filter(df, !is.na(UQE(enquo_x)))
    }
    
    #check number of levels and give appropriate errors
    if (levelError==T){
      l <- length(levels(factor(df[[quo_name(enquo_x)]])))
      if (l>25){
        stop(paste(quo_name(enquo_x),"contains more than", levelErrorNumber, "levels (has", length(levels(factor(df[[quo_name(enquo_x)]]))), "levels). Set levelError=FALSE to proceed."))
      }
    }
    
    #Calculate frequencies
    df<-  df %>%
      count(factor(UQE(enquo_x)), sort=sortResults) %>%
      mutate(percentage = n/sum(n),
             cumulative = cumsum(n),
             cumulative_percent = cumulative/sum(n)
      )
    if (sortResults==T){
      df[1] <- factor(df[[1]], levels = df[[1]][order(-df$n)])
    }
    
    #Format name for printing
    n <- names(df)
    n[1] <- quo_name(enquo_x)
    names(df) <- n
    
    #Set results class
    class(df) <- c("freqR_freq",class(df))

    #Print results as requested
    if (printResults==T){
      attr(df, "title") <- quo_name(enquo_x)
      print(df)
      
      if (na.rm==T){
        naCount1 <- nrow(naCount)
        naPercent<-(naCount1/nrow(df_orig))*100
        cat(paste0("NA's excluded: ", prettyNum(naCount1, big.mark=","), " (", formatC(naPercent, digits=1, format="f"), "%)\n\n"))
      }
    }
    
    #Plot results
    if (plotResults==T){
      gg <- ggplot(data=df, aes_string(quo_name(enquo_x), "n"))
      gg <- gg + geom_bar(stat="identity")
      gg <- gg + theme_minimal() + ggtitle (paste(quo_name(enquo_x),"Frequency")) + ylab("Count")
      gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      print (gg)
    }

  }
  
  
  #Save results as requested
  if (saveResults==T){
    return(df)
  }
  else{
    invisible(NULL)
  }
}

