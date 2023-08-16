#' Construct frequency table
#'
#' @description
#' Prints a frequency table for the specified variable.
#'
#' @details
#' This function constructs a frequency table for the specified variable. It expects
#' a data frame as the first argument and an (unquoted) variable as the second argument.
#' It will thus fit into a tidyverse pipeline.  Alternatively, for convienience, a
#' vector can be passed as first argument and the var (second) argument left blank.
#'
#' The console printed table is highly customizable using session, project, or global
#' options.  The following options are currently implemented and can be set by calling
#' `options()` either interactively or by including in your .Rprofile.
#'
#' - SimpleFreqs.inner_table_padding
#' - SimpleFreqs.table_symbol
#' - SimpleFreqs.row_divider_symbol
#' - SimpleFreqs.print_table_symbol
#' - SimpleFreqs.print_table_total_row
#' - SimpleFreqs.print_table_metadata
#' - SimpleFreqs.print_header_divider
#' - SimpleFreqs.big_mark
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

freq <- function(df, var = NA, plot = TRUE, sort = TRUE, na.rm = FALSE) {
  # Check if df is vector or data frame.  Stop if not
  if (!is.atomic(df) & !is.data.frame(df)) {
    stop("The first argument must be a data frame or vector")
  }

  # Check if data frame. If not, translate to data frame (with specified var name)
  # This allows you to feed it either a vector or a data frame/variable combo.
  # So after this we have a dataframe (df) and a var even if we were just
  # fed a vector
  if (!is.data.frame(df)) {
    dfname <- deparse(substitute(df))

    # if in form df$var
    if (grepl("$", dfname, fixed = T)) {
      dfname <- strsplit(dfname, "$", fixed = T)[[1]][[2]]
      df <- data.frame(x = df)
      names(df) <- c(dfname)
      var <- rlang::sym(dfname)

      # if in form df[["var"]]
    } else if (grepl("\"", dfname, fixed = T)) {
      dfname <- strsplit(dfname, "\"", fixed = T)[[1]][[2]]
      df <- data.frame(x = df)
      names(df) <- c(dfname)
      var <- rlang::sym(dfname)

      # if just a vector vector
    } else {
      df <- data.frame(x = df)
      names(df) <- c(dfname)
      var <- rlang::sym(dfname)
    }
  }

  # Capture input variable for non-standard evaluation
  enquo_x <- rlang::enquo(var)

  # Capture input variable data for later printing
  var_class <- class(df[[rlang::get_expr(enquo_x)]])

  # Get NA count
  var_missing <- sum(is.na(df[[rlang::get_expr(enquo_x)]]))

  # remove NA if na.rm=T
  if (na.rm == TRUE) {
    naCount <- df %>%
      filter(is.na(!!rlang::get_expr(enquo_x)))
    df <- filter(df, !is.na(!!rlang::get_expr(enquo_x)))
    naCount1 <- nrow(naCount)
  }

  # The main meat of the function, calculate freqs here
  df <- df %>%
    mutate(temp = factor(!!enquo_x, exclude = NULL)) %>%
    count(.data$temp, sort = sort) %>% # .data$temp used to quiet R check note
    mutate(
      percentage = n / sum(n),
      cumulative = cumsum(n),
      cumulative_percent = .data$cumulative / sum(n), # .data$cumulative used to quiet R check note
    )

  # sort factor if option selected
  if (sort == TRUE) {
    df[1] <- factor(df[[1]], exclude = NULL, levels = df[[1]][order(-df$n)])
  }

  # Format name for dataframe
  names(df) <- c(quo_name(enquo_x), "freq", "proportion", "cum_freq", "cum_proportion")

  # Set results class and attributes
  class(df) <- c("SimpleFreqs_freq", "data.frame")
  attr(df, "title") <- quo_name(enquo_x)
  if (na.rm == TRUE) {
    attr(df, "missing") <- naCount1
    attr(df, "missing_removed") <- TRUE
  }
  attr(df, "missing") <- var_missing
  attr(df, "varClass") <- var_class

  # Plot results
  if (plot == TRUE) {
    gg <- ggplot(data = df, aes_string(quo_name(enquo_x), "freq"))
    gg <- gg + geom_bar(stat = "identity")
    gg <- gg + theme_minimal() + ggtitle(paste("Frequency:", quo_name(enquo_x))) + ylab("Count")
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(gg)
  }

  return(df)
}
