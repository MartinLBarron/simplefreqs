#' listFreq
#' 
#' @title Frequency list of multiple variables crossed
#'
#' @description \code{listFreq} prints a frequency table of the cross of each
#' variable
#'
#' @details This function takes a list of variables and gives counts for each
#' group. For example, if variable A had two levels (1,2) and variable B had 2
#' levels (4,6) then listcount would give counts of cases with 1,4, 1,6, 2,4,
#' and 2,6.
#'
#' @param df A dataframe.
#' @param ... A list of variables.
#' @param plotResults if TRUE (default) prints bar chart of results.  If FALSE, no chart.
#' @param saveResults If FALSE (default) returns original dataframe. If TRUE,
#'   returns frequency results as a dataframe. This allows user to place this
#'   function inside a dplyr chain
#' @param sortResults If FALSE (default), prints results in cross order.  Otherwise,
#'   if TRUE, sort results by frequency.
#' @param printResults If TRUE (default), prints results to console.  Otherwise,
#'   if FALSE, no results are printed.
#' @param levelError if TRUE (default) gives an error if the variable passed
#'   has more than 25 levels. If
#' @param na.rm if TRUE (default) NAs are included in frequency list.  If FALSE,
#'   NA are removed (but reported seperately)
#' @return The original dataframe or table containing frequencies, Produces
#'   side-effect of printed frequencie table
#'

#' @examples
#' \dontrun{
#'listFreq(mtcars, cyl, gear)
#'}
#' @import dplyr
#' 
#' @export

listFreq <- function(df,..., plotResults=T, saveResults=F, printResults=T, sortResults=F, levelError=T, na.rm=F){

  #capture input variables
  enquo_vars <- quos(...)
  
  #save original dataset for later
  df_orig <- df
  
  #limit dataset to only needed variables
  df <- select(df, !!!enquo_vars)

  #remove NA if na.rm=T
  if (na.rm==T){
    df <- df[complete.cases(df), ]
    naCount <- nrow(df_orig) - nrow(df)
  }

  #check number of levels and give appropriate errors
  levelErrorNumber <- 200
  if (levelError==T){
    dftemp <-  df %>%
      group_by(!!!enquo_vars) %>%
      filter(row_number()==1)
    l <-nrow(dftemp)
    if (l>levelErrorNumber){
      stop(paste("Produced table contains more than", levelErrorNumber,"levels  (has", l, "levels). Set levelError=FALSE to proceed."))
    }
  }
  
  #Calculate frequencies
  df <-  df %>%
    group_by(!!!enquo_vars) %>%
    summarize(n=n()) %>%
    ungroup()

  df <- df %>%
  mutate(percentage = n/sum(n))

  
  # df <-  dfgss %>%
  #   group_by(year, race,marital) %>%
  #   summarize(n=n()) 
  # 
  # df <- df %>%
  #   mutate(inner_percentage = n/sum(n),
  #          test1=sum(n))
  # 
  # df <- df %>%
  #   ungroup() %>%
  #   mutate(outer_percentage = n/sum(n),
  #          test2=sum(n))
  # 
  

  #Format for printing
  # dfprint <-  df %>%
  #   mutate(n = formatC(n, format="f", digits=0, big.mark=","),
  #          percentage = paste0(formatC(100 * percentage, digits=1, format="f"), "%")
  #   )
  
  if (sortResults==T){
    df <- arrange(df, desc(n))
  }

  #Print results as requested
  if (printResults==T){
    dfprint <- df
    class(dfprint) <- c("freqR_listFreq",class(dfprint))
    print(dfprint)
  }
  
  if (na.rm==T){
    naCount1 <- nrow(naCount)
    naPercent<-(naCount1/nrow(df_orig))*100
    cat("______________________\n")
    cat(paste0("NA excluded (", prettyNum(naCount1, big.mark=","), ", ", formatC(naPercent, digits=1, format="f"), "%)"))
  }

  #Save results as requested
  if (saveResults==T){
    return(df)
  }
  else{
    invisible(NULL)
  }
  
}


# library(forcats)
# library(dplyr)
# dfgss <-gss_cat

#Calculate frequencies
# df <-  dfgss %>%
#   group_by(year, race, marital) %>%
#   summarize(n=n())
# 
# df <- df %>%
#   mutate(percentage = n/sum(n))


#check <- listFreq(dfgss, year, race,marital, levelError =FALSE)









