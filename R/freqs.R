#' freqs
#'
#' @title Caclulate frequencies of multiple variable
#'
#' @description
#' \code{freqs} prints a frequency table for each variable passed to it.
#'
#' @details
#' This function prints a frequency table for variables passed to it.  In
#' order to fit within the tidyverse, it takes as its first argument a dataframe.
#' Unline \code{freq} it returns null. 
#'
#' @param df A dataframe (You can also pass this program a single variable and it will silently transform it to a dataframe)
#' @param ... a variable or variables in associated dataframe. If you pass no variables, freq will be run on all variables in dataframe
#' @param plot if TRUE (default) prints bar chart of results.  If FALSE, no chart.
#' @param sort If TRUE (default), sort output in descending order of n. If FALSE, sort output in ascending order of levels
#' @param na.rm if FALSE (default) NAs are included in frequency list.  If TRUE, NA are removed (but reported seperately)
#' @return  Null
#' @examples
#' \dontrun{
#' freqs(iris, Species, Sepal.Width)
#'}
#' @import dplyr
#' @import ggplot2
#' @import rlang
#'
#' @export
#'

freqs <- function(df, ..., plot=T, sort=T, na.rm=F){

  #check if dataframe. If not, translate to dataframe
  if (!is.data.frame(df)){
    dfname <- deparse(substitute(df))
    if (grepl("$", dfname, fixed=T)){
      dfname <- strsplit(dfname, "$", fixed=T)[[1]][[2]]
      df <- data.frame(x=df)
      names(df)<-c(dfname)
    } else{
      dfname <- strsplit(dfname, "\"", fixed=T)[[1]][[2]]
      df <- data.frame(x=df)
      names(df)<-c(dfname)
    }
  }
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
    #Capture input variable for non-standard evaluation
    enquo_x <- var

    #remove NA if na.rm=T
    if (na.rm==T){
      naCount <- df %>%
        filter(is.na(!! get_expr(enquo_x)))
      df=filter(df, !is.na(!! get_expr(enquo_x)))
    }

    df<-  df  %>%
      mutate (temp=factor(!! get_expr(enquo_x),exclude=NULL)) %>%
      count(temp, sort=sort)  %>%
      mutate(percentage = (n/sum(n))*100,
             cumulative = cumsum(n),
             cumulative_percent = (cumulative/sum(n))*100
      )

    if (sort==T){
      df[1] <- factor(df[[1]], levels = df[[1]][order(-df$n)])
    }

    #Format name for printing
    n <- names(df)
    names(df) <- c(quo_name(enquo_x), "Freq", "Percent", "CumFreq", "CumPercent")

    #Set results class
    #class(df) <- c("freqR_freq",class(df))
    #print(class(df))
    class(df) <- c("freqR_freq", "data.frame")

    attr(df, "title") <- quo_name(enquo_x)

    #Print results as requested
    if (na.rm==T){
      naCount1 <- nrow(naCount)
      attr(df, "MissingRemoved") <- naCount1
     }

    print(df)

    #Plot results
    if (plot==T){
      gg <- ggplot(data=df, aes_string(quo_name(enquo_x), "Freq"))
      gg <- gg + geom_bar(stat="identity")
      gg <- gg + theme_minimal() + ggtitle (paste("Frequency:", quo_name(enquo_x))) + ylab("Count")
      gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      print (gg)
    }

  }

  invisible(NULL)
}

