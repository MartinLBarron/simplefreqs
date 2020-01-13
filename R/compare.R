#' Produces a comparison table
#'
#' @param df a dataframe containing the variables to summarize
#' @param ... one or more variables to summarize
#' @param group_var if given, statistics will be computed for each level of groupby variable
#' @param types types of summary statistics to computed (as vector of strings). Default is all of the following: "n", "nobs", "nmiss", "sum","mean","sd", "min", "p1", "p5", "p25", "median", "p75", "p95", "p99", "max"
#' @param long if TRUE (default), aranges statistics in columns and variable in rows Otherwise, shows reverse
#' @param na.rm whether or not na.rm==T is passsed to summarizing functions.
#'
#' @return Returns a dataframe containing summary statistics.
#' 
#' @examples
#' \dontrun{}
#' compare(df, mpg, cyl)
#' 
#' @import dplyr
#' 
#' @export

compare <- function(df, ...,
                    group_var=NA,
                    types = c("n", "nobs", "nmiss","mean","sd", "min", "p25", "median", "p75", "max"),
                    long=F,
                    na.rm=T){
  
  enquo_group_by <- enquo(group_var)
  
  
  enquo_vars <- quos(...)
  
  #limit dataset to only needed variables
  if (is.na(quo_name(enquo_group_by))){
    df <- select(df, !!!enquo_vars)
  }else{
    df <- select(df, !!!enquo_vars, !!enquo_group_by)
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


  #setup container dataframe to hold results
  results <-data.frame()
  counter=0

  #We run separate code depending on whether they requested a group by variable or not
  if (is.na(quo_name(enquo_group_by))){

    #Loop over summary statistics for all variables in dataset
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
      counter=counter+1
      x <- df %>%
        group_by(!! enquo_group_by) %>%
        summarize_all(type, na.rm=na.rm) %>%
        mutate(type=type, sort=counter)
      results <- bind_rows(results, x)
    }

    results <- arrange(results,!!enquo_group_by,sort) %>%
    select(-sort)
    #rearrange columns and rows
    results <- results[, c(1, length(results), seq(2,length(results)-1))]

  }

  #Now rename n variable for printing
  results <-mutate(results, type=ifelse(type=="myN", "N", type))

  if (long==F){
    rownames(results) <- results$type
    results <- select(results, -type)
    results <- t(results)
    results <- as.data.frame(results)
    results$Variables <- row.names(results)
    row.names(results) <- NULL
    results <- select(results, Variables, everything())
  }

  class(results) <- c("freqR_compare",class(results))
  return(results)
}

