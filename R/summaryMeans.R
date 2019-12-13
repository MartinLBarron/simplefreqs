#' Produces summary statistics table
#'
#' @param df a dataframe containing the variables to summarize
#' @param ... one or more variables to summarize
#' @param types types of summary statistics to computed (as vector of strings). Default is all of the following: "n", "nobs", "nmiss", "sum","mean","sd", "min", "p1", "p5", "p25", "median", "p75", "p95", "p99", "max"
#' @param group_by if given, statistics will be computed for each level of groupby variable
#' @param saveResults if TRUE, dataframe of results is returned.  If FAlSE (default) original dataframe is returned
#' @param printResults if TRUE (default), summary table written to console.  If FALSE no results written to console.
#' @param na.rm whether or not na.rm==T is passsed to summarizing functions.
#'
#' @return dataframe
#' #'
#' @examples
#' \dontrun{}
#'
#' @import dplyr
#' @export summaryMeans

summaryMeans <- function(df, ...,
                         group_by=NA,
                         types = c("n", "nobs", "nmiss","mean","sd", "min", "p25", "median", "p75", "max"),
                         saveResults=F,
                         printResults=T,
                         na.rm=T, 
                         transpose=T){

  group_by <- enquo(group_by)


  enquo_vars <- quos(...)

  #Save df as is for later return
  df_orig=df

  #limit dataset to only needed variables
  if (is.na(quo_name(group_by))){
  df <- select(df, !!!enquo_vars)
  }else{
    df <- select(df, !!!enquo_vars, !!group_by)
  }

  #An n function conflicts with dplyr, so we capture n and rename it myN so user can still request N
  types <- ifelse(types=="n", "myN", types)

  #Setup helper functions
  myN <- function(x, na.rm=F) length(x)
  nobs <- function(x, na.rm=F) myN(x)-nmiss(x)
  nmiss<-function(x,na.rm=F) sum(is.na(x))
  p25 <-function(x, na.rm=F) quantile(x,.25, na.rm=na.rm)
  p75 <- function(x,na.rm=F) quantile(x,.75, na.rm=na.rm)
  p1 <- function(x,na.rm=F) quantile(x,.1, na.rm=na.rm)
  p99 <- function(x,na.rm=F) quantile(x,.99, na.rm=na.rm)
  p95 <- function(x,na.rm=F) quantile(x,.95, na.rm=na.rm)
  p5 <- function(x,na.rm=F) quantile(x,.5, na.rm=na.rm)

  results <-data.frame()
  counter=0
  if (is.na(quo_name(group_by))){

    for (type in types){
      counter=counter+1
      x <- df %>%
        summarize_all(type, na.rm=na.rm) %>%
        mutate(type=type, sort=counter)
      results <- bind_rows(results, x)
    }
    results <- arrange(results,sort) %>%
      select(-sort)
    #rearrange columns and rows
    results <- results[, c( length(results), seq(1,length(results)-1))]

  }
  else {
    for (type in types){
      x <- df %>%
        group_by(!!group_by) %>%
        summarize_all(type, na.rm=na.rm) %>%
        mutate(type=type, sort=counter)
      results <- bind_rows(results, x)
    }
    results <- arrange(results,!!group_by,sort) %>%
      select(-sort)
    #rearrange columns and rows
    results <- results[, c(1, length(results), seq(2,length(results)-1))]

  }

  #Now rename n variable for printing
  results <-mutate(results, type=ifelse(type=="myN", "N", type))

  #Left over from when I set formattin here
  # resultsA <- as.data.frame(results[,1])
  # names(resultsA)<-c("Statistic")
  # resultsB <- as.data.frame(results[,2:length(results)])
  # results <-bind_cols(resultsA,resultsB)
  # for (i in 1:length(resultsB)){
  #   resultsB[,i]<-comma(resultsB[,i], digits = 1)
  # }

  if (transpose==T){
    rownames(results) <- results$type
    results <- select(results, -type)
    results <- t(results)
    results <- as.data.frame(results)
    results$Variables <- row.names(results)
    row.names(results) <- NULL
    tt<<-results
    results <- select(results, Variables, everything())
    t2<<-results
  }

  #Print results as requested
  if (printResults==T){
    dfprint <- as.data.frame(results)
    class(dfprint) <- c("freqR_summaryMeans",class(results))
    print(dfprint)
  }

  #Save results as requested
  if (saveResults==T){
    class(results) <- c("freqR_summaryMeans",class(results))
    return(results)
  } else{
    return(df_orig)
  }
}

