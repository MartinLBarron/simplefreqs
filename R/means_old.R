#' 
#' #Required packages
#' #devtools::install_github("tidyverse/dplyr")
#' library(dplyr)
#' 
#' #' @title Caclulate cross frequencies of variables
#' #'
#' #' @description
#' #' \code{freq} prints a frequency table of the cross of each variable
#' #' 
#' #' @details
#' #' This function prints a frequency table for variables passed to it.  
#' #'
#' #' @param df A dataframe.
#' #' @param ... A list of variables.
#' #' @param saveResults If FALSE (default) returns original dataframe. If TRUE, returns frequency results as a dataframe. This allows user to place this function inside a dplyr chain
#' #' @param printResults If TRUE (default), prints results to console.  Otherwise, if FALSE, no results are printed.
#' #' @param sortResults If TRUE (default), sort output in descending order of n. If FALSE, sort output in ascending order of levels
#' #' @param levelWarning if TRUE (default) gives an error if the variable passed has more than 25 levels. If
#' #' @param na.rm if TRUE (default) NAs are included in frequency list.  If FALSE, NA are removed (but reported seperately)
#' #' @return The original dataframe or table containing frequencies, Produces side-effect of printed frequencie table
#' 
#' 
#' 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' 
#' 
#' listCount <- function(df,..., saveResults=F, printResults=T, sortResults=T, levelWarning=T, na.rm=F){
#'   
#'   # df <- dfgss
#'   # my_var <- quo(year)
#'   # vars <- quos(...)
#'   # 
#'   # #Capture input variable for non-standard evaluation
#'   # enquo_x <- enquo(x)
#'   enquo_vars <- quos(...)
#'   
#'   #Save df as is for later return
#'   df_orig=df
#'   
#'   #limit dataset to only needed variables
#'   df <- select(df, !!!enquo_vars)
#'   
#'   #remove NA if use.NA=F
#'   # if (na.rm==T){
#'   #   naCount <- df %>%
#'   #     filter(is.na(UQE(enquo_x)))
#'   #   df=filter(df, !is.na(UQE(enquo_x)))
#'   # }
#'   
#'   #check number of levels and give appropriate errors
#'   if (levelWarning==T){
#'     dftemp <-  df %>%
#'       group_by(!!!enquo_vars) %>%
#'       filter(row_number()==1)
#'     l <-nrow(dftemp)
#'     if (l>50){
#'       stop(paste("Variables contains more than 50 levels (has", l, "levels). Set warning=FALSE to proceed."))
#'     }
#'   }
#'   
#'   #Calculate frequencies
#'   df <-  df %>%
#'     group_by(!!!enquo_vars) %>%
#'     summarize(n=n()) %>%
#'     mutate(inner_percentage = n/sum(n)) %>%
#'     ungroup()
#'   
#'   df <- df %>%
#'   mutate(outer_percentage = n/sum(n)) 
#'   
#' 
#'   #Format for printing
#'   dfprint <-  df %>%
#'     mutate(n = formatC(n, format="f", digits=0, big.mark=","),
#'            inner_percentage = paste0(formatC(100 * inner_percentage, digits=1, format="f"), "%"),
#'            outer_percentage = paste0(formatC(100 * outer_percentage, digits=1, format="f"), "%")
#'     )
#'   # n <- names(dfprint)
#'   # n[1] <- quo_name(enquo_x)
#'   # names(dfprint) <- n
#' 
#'   
#'   #Print results as requested
#'   if (printResults==T){
#'     dfprint <- as.data.frame(dfprint)
#'     print(dfprint, justify="left", row.names=F)
#'   }  
#'   
#'   if (na.rm==T){
#'     naCount1 <- nrow(naCount)
#'     naPercent<-(naCount1/nrow(df_orig))*100
#'     cat("______________________\n")
#'     cat(paste0("NA excluded (", prettyNum(naCount1, big.mark=","), ", ", formatC(naPercent, digits=1, format="f"), "%)"))
#'   }
#'   
#'   #Save results as requested
#'   if (saveResults==T){
#'     return(df)
#'   } else{
#'     return(df_orig)
#'   }
#' }
#' 
#' check <- listCount(dfgss, year, race)
#' 
#' 
#' 
#' 
#'   
#' 
#' mbsummarize <-function(var){
#'   
#'   var_nobs <- var  %>%
#'     summarize(FYE_2009=n(),
#'               FYE_2011=n(),
#'               FYE_2014=n())%>%
#'     mutate (type="nobs",
#'             order=1)
#'   var_n  <- var  %>%
#'     summarize(FYE_2009=sum(!is.na(FYE_2009)),
#'               FYE_2011=sum(!is.na(FYE_2011)),
#'               FYE_2014=sum(!is.na(FYE_2014)))%>%
#'     mutate (type="n",
#'             order=2)
#'   
#'   var_sum <- var  %>%
#'     summarize(FYE_2009=sum(FYE_2009, na.rm=T),
#'               FYE_2011=sum(FYE_2011, na.rm=T),
#'               FYE_2014=sum(FYE_2014, na.rm=T)) %>%
#'     mutate (type="sum",
#'             order=3)
#'   
#'   var_mean<- var  %>%
#'     summarize(FYE_2009=mean(FYE_2009, na.rm=T),
#'               FYE_2011=mean(FYE_2011, na.rm=T),
#'               FYE_2014=mean(FYE_2014, na.rm=T)) %>%
#'     mutate (type="mean",
#'             order=4)
#'   
#'   var_sd <- var  %>%
#'     summarize(FYE_2009=sd(FYE_2009, na.rm=T),
#'               FYE_2011=sd(FYE_2011, na.rm=T),
#'               FYE_2014=sd(FYE_2014, na.rm=T)) %>%
#'     mutate (type="sd",
#'             order=5)
#'   
#'   
#'   var_min <- var  %>%
#'     summarize(FYE_2009=min(FYE_2009, na.rm=T),
#'               FYE_2011=min(FYE_2011, na.rm=T),
#'               FYE_2014=min(FYE_2014, na.rm=T)) %>%
#'     mutate (type="min",
#'             order=6)
#'   
#'   var_median <- var  %>%
#'     summarize(FYE_2009=median(FYE_2009, na.rm=T),
#'               FYE_2011=median(FYE_2011, na.rm=T),
#'               FYE_2014=median(FYE_2014, na.rm=T)) %>%
#'     mutate (type="median",
#'             order=7)
#'   
#'   
#'   var_max <- var  %>%
#'     summarize(FYE_2009=max(FYE_2009, na.rm=T),
#'               FYE_2011=max(FYE_2011, na.rm=T),
#'               FYE_2014=max(FYE_2014, na.rm=T)) %>%
#'     mutate (type="max",
#'             order=8)
#'   
#'   var_range <- var  %>%
#'     summarize(FYE_2009=range(FYE_2009, na.rm=T)[2]-range(FYE_2009, na.rm=T)[1],
#'               FYE_2011=range(FYE_2011, na.rm=T)[2]-range(FYE_2011, na.rm=T)[1],
#'               FYE_2014=range(FYE_2014, na.rm=T)[2]-range(FYE_2014, na.rm=T)[1]) %>%
#'     mutate (type="range",
#'             order=9)
#'   
#'   
#'   var <- bind_rows(var_sum, var_mean, var_sd, var_min, var_max, var_median, var_range, var_nobs, var_n) %>%
#'     arrange(order) %>%
#'     select(-order)
#'   return(var)
#' }
#' 
#' 
#' 
#' 
