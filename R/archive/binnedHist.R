#' @title  Create a ggplot histogram object with binned outliers
#'
#' @description \code{binnedHist} creates a histogram with specified outliers
#' binned.
#'
#' @details This creates a histogram.  Upper and/or lower outliers are grouped
#' together and shown in a different color to differentiate from regular
#' histogram bars.
#'
#' @param df a dataframe containing the variable to be ploted
#' @param var_name the variable to be plotted
#' @param bottomCode value below which should be binned
#' @param topCode value above which should be binned
#' @param col outline color for bars (default black)
#' @param fill fill color for bars (dfault blue)
#' @param fill_outlier_bins fill color for outlier bins
#' @param binwidth width of bins
#'
#' @return Returns a ggplot object
#'
#' @examples
#' binnedHist(iris,"Sepal.Length", 5, 7)
#'
#' @import dplyr
#' @import ggplot2
#'

binnedHist <- function(df, var_name,bottomCode=NA,topCode=NA,col = "black",fill = "cornflowerblue",fill_outlier_bins = "forestgreen", binwidth = NULL) {

  #get the min and max of the variable
  printing_min_max <- df %>% summarise_(sprintf("round(min(%s, na.rm = TRUE), 1)", var_name),
                                       sprintf("round(max(%s, na.rm = TRUE), 1)", var_name))

  #construct filters for ceiling and floor
  ceiling_filter <- ifelse(!is.na(topCode),
                           sprintf("%s < %f", var_name, topCode),
                           "1 == 1")
  floor_filter   <- ifelse(!is.na(bottomCode),
                           sprintf("%s > %f", var_name, bottomCode),
                           "1 == 1")

  #filter dataset to only data in the range
  df_regular <- df %>% filter_(ceiling_filter, floor_filter) %>%
    select_(var_name)

  #roll up data above ceiling
  df_to_roll_ceiling <- df %>% filter_(
    sprintf("%s >= %f", var_name, topCode)) %>% select_(var_name)
  if (!is.na(topCode)) df_to_roll_ceiling[, 1] <- topCode

  #roll up date below ceiling
  df_to_roll_floor <- df %>% filter_(
    sprintf("%s <= %f", var_name, bottomCode)) %>% select_(var_name)
  if (!is.na(bottomCode)) df_to_roll_floor[, 1] <- bottomCode

  #create plot object for just the regular cases
  plot_obj <- ggplot(df_regular, aes_string(var_name)) +
    geom_histogram(col = col, fill = fill, binwidth = binwidth)


  #for ceiling, add the ceiling bin and fix label
  if (!is.na(topCode)) {
    ticks_for_ceiling <- update_tickmarks_ceiling(plot_obj, topCode,
                                                  printing_min_max[1,2])
    plot_obj <- plot_obj +
      geom_histogram(data = df_to_roll_ceiling, fill = fill_outlier_bins, col = col,
                     binwidth = binwidth) +
      scale_x_continuous(breaks = ticks_for_ceiling$tick_positions,
                         labels = ticks_for_ceiling$tick_labels)+
      theme_minimal()
  }
  #for floor, add the floor bid and fix labels
  if (!is.na(bottomCode)) {
    ticks_for_floor <- update_tickmarks_floor(plot_obj, bottomCode,
                                              printing_min_max[1,1])
    suppressMessages(plot_obj <- plot_obj +
                       geom_histogram(data = df_to_roll_floor, fill = fill_outlier_bins,
                                      col = col, binwidth = binwidth) +
                       scale_x_continuous(breaks = ticks_for_floor$tick_positions,
                                          labels = ticks_for_floor$tick_labels) +
                       theme_minimal()

                     )
  }

  return(plot_obj)
}

## Helper functions
update_tickmarks_ceiling <- function(gg_obj,co,max_print) {
  ranges <- suppressMessages(
    ggplot_build(gg_obj)$layout$panel_ranges[[1]])
  label_to_add <- sprintf("(%s , %s)", round(co, 1), max_print)
  tick_positions <- ranges$x.major_source
  tick_labels    <- ranges$x.labels
  if (overlap_ceiling(tick_positions, co)) {
    tick_positions <- tick_positions[-length(tick_positions)]
    tick_labels    <- tick_labels[-length(tick_labels)]
  }
  return(list(tick_positions = c(tick_positions, co),
              tick_labels    = c(tick_labels, label_to_add)))
}

overlap_ceiling <- function(positions, cut_off) {
  n <- length(positions)
  ticks_dif <- positions[n] - positions[n-1]
  (cut_off - positions[n]) / ticks_dif < 0.25
}

update_tickmarks_floor <- function(gg_obj, co, min_print) {
  ranges <- suppressMessages(
    ggplot_build(gg_obj)$layout$panel_ranges[[1]])
  label_to_add <- sprintf("(%s , %s)", min_print, round(co, 1))
  tick_positions <- ranges$x.major_source
  tick_labels    <- ranges$x.labels
  if (overlap_floor(tick_positions, co)) {
    tick_positions <- tick_positions[-1]
    tick_labels    <- tick_labels[-1]
  }
  return(list(tick_positions = c(co, tick_positions),
              tick_labels    = c(label_to_add, tick_labels)))
}

overlap_floor <- function(positions, cut_off) {
  ticks_dif <- positions[2] - positions[1]
  (positions[1] - cut_off) / ticks_dif < 0.25
}


# library(ggplot2)

# library(tidyverse)
# set.seed(42)
# hist_data <- data_frame(x = c(rexp(1000, .5),
#                               runif(50, 0, 500)))
#
#
# binnedHist(hist_data, "x", topCode=55, binwidth = 5)


# ggplot(hist_data, aes(x)) +
#   geom_histogram(binwidth = .1, col = "black", fill = "cornflowerblue")
#
#
# df <- data_frame(x = c(runif(100, 0, 100), rnorm(1000, 50, 2)))
#
#
# gg <- data_frame(x = c(runif(100, 0, 100), rnorm(1000, 50, 2))) %>%
#   mb.hist("x", 40, 60, binwidth = 1)
#
# gg + theme_minimal()

# x = hist_data
# var_name="x"
# bottomCode=45
# topCode=55
# binwidth=.1
# col = "black"
# fill = "cornflowerblue"
# fill_outlier_bins = "forestgreen"
