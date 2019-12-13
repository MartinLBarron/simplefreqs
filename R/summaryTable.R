#' SummaryTable
#' 
#' @title Construct a summary table of statistics
#'
#' @description
#' prints a summary table crossing two 
#' 
#' @param df A dataframe
#' @param var a variable in associated dataframe.
#' @param x col variable
#' @param y row variable
#' @param function_name function to be used to summarize results
#' @param ... other options to pass to function
#' @return Dataframe containing table
#' @import dplyr
#' @import tidyr
#' 
#' @export
#'


summaryTable <- function(df, var, x, y, function_name, ...){
  
  # df <- df_comp
  # var<- quo(FTAflag)
  # y <- quo(MaxPSAFTAS)
  # x <- quo(GunCharge)
  
  #enquo input variables
  var <- enquo(var)
  x <- enquo(x)
  y <- enquo(y)
  
  #function to apply generic function
  apply_some_function <- function(data, function_name, ...){
    FUN<-match.fun(function_name)
    FUN(data,...)
  }
  
  #get overall score
  overall <- df %>%
    summarize (calc=apply_some_function((!!var), function_name,...)) 
  
  #Calculate column marginals
  colMargin <- df %>%
    group_by(!!y) %>%
    summarize (calc=apply_some_function((!!var), function_name,...)) %>%
    ungroup()
  
  #calculate row marginals
  rowMargin <- df %>%
    group_by(!!x) %>%
    summarize (calc=apply_some_function((!!var), function_name,...)) %>%
    ungroup() %>%
    spread(!!x, calc)
  
  #add overall score to rowmargin
  rowMargin <- bind_cols(rowMargin, overall)
  
  #calc overall table
  tab <- df %>%
    group_by(!!x,!!y) %>%
    summarize (calc=apply_some_function((!!var), function_name,...)) %>%
    ungroup() %>%
    spread(!!x, calc)
  
  
  #combine marginal and overall tables
  #col
  tab <- inner_join(tab, colMargin)
  
  tab <- bind_rows(tab, rowMargin)
  
  #clean up labels:
  tab[nrow(tab),1]<-"Overall"
  
  nmes <- names(tab)
  nmes[length(nmes)] <- "Overall"
  names(tab) <- nmes
  
  return(tab)
}


