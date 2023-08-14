#' Construct frequency table
#' 
#' @description
#' Prints a frequency table for the specified variable.
#' 
#' @details
#' This function prints a frequency table for the specified variable. While the 
#' order to fit within the tidyverse, it takes as its first argument a dataframe
#' and returns the frequency table as a data frame. 
#'
#' @param df A data frame (optionally, you can pass a variable as the first argument.)
#' 
#' @param var a variable in associated data frame
#' 
#' @param plot if TRUE (default) prints bar chart of results.  If FALSE, no chart.
#' 
#' @param sort If TRUE (default), sort output in descending order of n. If FALSE, sort output in ascending order of levels
#' 
#' @param na.rm if FALSE (default) NAs are included in frequency list.  If TRUE, NA are removed (but reported separately)
#'
#' @return  data frame containing frequencies.
#' 
#' @examples
#' 
#' freq(iris, Species)
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @export
#' 

freq <- function(df, var=NA, plot=T, sort=T, na.rm=F){
  
  if (!is.atomic(df) & !is.data.frame(df)){
    stop("The first argument must be a data frame or vector")
  }
  
  #check if dataframe. If not, translate to dataframe
  #This allows you to feed it either a vector or a dataframe/variable combo
  if (!is.data.frame(df)){
    dfname <- deparse(substitute(df))
    # if in form df$var
    if (grepl("$", dfname, fixed=T)){
      dfname <- strsplit(dfname, "$", fixed=T)[[1]][[2]]
      df <- data.frame(x=df)
      names(df)<-c(dfname)
      var <- rlang::sym(dfname)
    # if in form df[["var"]]
    } else if (grepl("\"", dfname, fixed=T)){
      dfname <- strsplit(dfname, "\"", fixed=T)[[1]][[2]]
      df <- data.frame(x=df)
      names(df)<-c(dfname)
      var <- rlang::sym(dfname)
    # if just a vector vector
    } else{
      df <- data.frame(x=df)
      names(df)<-c(dfname)
      var <- rlang::sym(dfname)
    }
  } 

  # testing

  #Capture input variable for non-standard evaluation
  enquo_x <- enquo(var)
  
  #Capture input variable class for later printing
  #var_class <- class(df[[var]])
  
  #remove NA if na.rm=T
  if (na.rm==T){
    naCount <- df %>%
      filter(is.na(!! get_expr(enquo_x)))
    df=filter(df, !is.na(!! get_expr(enquo_x)))
    naCount1 <- nrow(naCount)
  }
  
  # The main meet of the function, calculate freqs here
  df<-  df  %>%
    mutate (temp = factor(!! enquo_x,exclude=NULL)) %>%
    count(.data$temp, sort=sort)  %>% #.data$temp used to quiet R check note
    mutate(percentage = (n/sum(n))*100,
           cumulative = cumsum(n),
           cumulative_percent = (.data$cumulative/sum(n))*100 #.data$cumulative used to quiet R check note
    )
  
  #sort factor for chart
  if (sort==T){
    df[1] <- factor(df[[1]], levels = df[[1]][order(-df$n)])
  }
  
  #'     
  #Format name in dataframe
  names(df) <- c(quo_name(enquo_x), "Freq", "Percent", "CumFreq", "CumPercent")
  
  #Set results class
  class(df) <- c("SimpleFreqs_freq", "data.frame")
  attr(df, "title") <- quo_name(enquo_x)
  if (na.rm==T){
    attr(df, "MissingRemoved") <- naCount1
  }
  #Plot results
  if (plot==T){
    gg <- ggplot(data=df, aes_string(quo_name(enquo_x), "Freq"))
    gg <- gg + geom_bar(stat="identity")
    gg <- gg + theme_minimal() + ggtitle (paste("Frequency:", quo_name(enquo_x))) + ylab("Count")
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print (gg)
  }
  
  return(df)
}

