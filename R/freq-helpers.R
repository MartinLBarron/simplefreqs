# helpers and print functions for class


# Check if vector is integer ----------------------------------------------
checkIfInteger <- function(x) {
  suppressWarnings(
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
  )
  if (test == TRUE) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Determine Max width of column ------------------------------------------
DetermineColumnWidth <- function(x) {
  # x <- as.character(x)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- max(sapply(x, nchar), na.rm = T)
  return(x)
}

# Determine width of all columns ------------------------------------------
DetermineColumnWidths <- function(df, footer) {
  # Determine width of data
  data <- sapply(df, DetermineColumnWidth)
  
  # Determine width of headers
  header <- names(df)
  header <- sapply(header, DetermineColumnWidth)
  
  # determine width of footer (total row)
  footer <- sapply(footer, DetermineColumnWidth)
  
  # determine max of header, data, and footer
  results <- ifelse(header > data, header, data)
  results <- ifelse(results > footer, results, footer)
  return(results)
}

# Generic function to lay out table as desired ---------------------------------
print_console_helper <- function(df,
                                 inner_table_padding = getOption("simplefreqs.inner_table_padding", default = 5),
                                 row_divider_symbol = getOption("simplefreqs.row_divider_symbol", default = "\u2500"),
                                 table_symbol = getOption("simplefreqs.table_symbol", default = "\u2550"),
                                 print_table_symbol = getOption("simplefreqs.print_table_symbol", default = TRUE),
                                 print_total_row = getOption("simplefreqs.print_table_total_row", default = TRUE),
                                 print_metadata = getOption("simplefreqs.print_table_metadata", default = TRUE),
                                 print_header_divider = getOption("simplefreqs.print_header_divider", default = TRUE)) {
  # Set Constants
  space_symbol <- " "
  big_mark <- getOption("simplefreqs.big_mark", default = ",")
  n <- sum(df$Freq)
  decimal_digits <- getOption("simplefreqs.decimal_digits", default = 1)
  
  # Replace NA with <NA> for printing
  # We check if <NA> alrady exist in data.frame and issues warning if it does
  lab <- levels(df[[1]])
  if ("<NA>" %in% lab) {
    warning('the string "<NA>" was detected. This conflicts with the printed NA results')
  }
  lab[is.na(lab)] <- "<NA>"
  levels(df[[1]]) <- lab
  
  
  missing <- attr(df, "na", exact = T)
  missingRemoved <- attr(df, "na_removed", exact = T)
  
  if (!is.null(missingRemoved)) {
    norig <- sum(df$Freq) + missing
  } else {
    norig <- sum(df$Freq)
  }
  naPercent <- (missing / norig) * 100
  
  # Determine if freqs are all integer or not (due to weighting)
  allInteger <- checkIfInteger(df[[2]])
  
  # Convert Dataframe to all character
  # format as specified
  df[1] <- as.character(df[[1]])
  if (allInteger == TRUE) {
    df[2] <- formatC(df[[2]], format = "f", digits = 0, big.mark = big_mark)
    df[4] <- formatC(df[[4]], format = "f", digits = 0, big.mark = big_mark)
  } else {
    df[2] <- formatC(df[[2]], format = "f", digits = 1, big.mark = big_mark)
    df[4] <- formatC(df[[4]], format = "f", digits = 1, big.mark = big_mark)
  }
  
  df[3] <- formatC(df[[3]] * 100, format = "f", digits = decimal_digits)
  
  df[5] <- formatC(df[[5]] * 100, format = "f", digits = decimal_digits)
  
  # Format Footer
  if (allInteger == TRUE){
    footer <- c(
      "Total",
      formatC(n, format = "f", digits = 0, big.mark = big_mark),
      "100%",
      "",
      "")
  } else {
    footer <- c(
      "Total",
      formatC(n, format = "f", digits = decimal_digits, big.mark = big_mark),
      "100%",
      "",
      ""
    )
  }
  
  maxColWidth <- DetermineColumnWidths(df, footer)
  # add margins to columns
  maxColWidth <- maxColWidth + inner_table_padding
  maxLength <- sum(maxColWidth)
  
  # get column names
  nme <- names(df)
  
  # Print Metadata ---------------------------------------------------------
  if (print_metadata == TRUE) {
    cat("\nVariable: ", attr(df, "title"), "\n", sep = "")
    cat("Class: ", attr(df, "varClass", exact = T), "\n", sep = "")
    
    if (!is.null(missingRemoved)) {
      cat(paste0("NAs (removed): ", prettyNum(missing, big.mark = big_mark), " (", formatC(naPercent, digits = decimal_digits, format = "f"), "%)\n"))
    } else {
      cat(paste0("NAs: ", prettyNum(missing, big.mark = big_mark), " (", formatC(naPercent, digits = decimal_digits, format = "f"), "%)\n"))
    }
  }
  # Print Table top ---------------------------------------------------------
  if (print_table_symbol == TRUE) {
    cat(rep(table_symbol, maxLength + 2), "\n", sep = "")
  }
  
  # Print Header ------------------------------------------------------------
  # padding for header labels
  for (col in 1:length(df)) {
    colSize <- maxColWidth[col]
    # determine padding
    cellSize <- nchar(nme[col])
    padding <- colSize - cellSize
    startPadding <- padding
    endPadding <- 0
    cat(rep(space_symbol, startPadding), nme[[col]], rep(space_symbol, endPadding), sep = "")
  }
  cat("\n")
  
  if (print_header_divider == TRUE) {
    cat(rep(row_divider_symbol, maxLength + 2), "\n", sep = "")
  }
  
  # Cell Values -------------------------------------------------------------
  for (row in 1:nrow(df)) {
    for (col in 1:length(df)) {
      colSize <- maxColWidth[col]
      # determine padding
      if (is.na(df[row, col])) {
        cellSize <- 2
      } else {
        cellSize <- nchar(df[row, col])
      }
      padding <- colSize - cellSize
      cat(rep(space_symbol, padding), df[[row, col]], sep = "")
    }
    cat("\n")
  }
  
  # Total Row ---------------------------------------------------------------
  
  if (print_total_row == T) {
    cat(rep(row_divider_symbol, maxLength + 2), "\n", sep = "")
    
    for (col in 1:length(df)) {
      colSize <- maxColWidth[col]
      # determine padding
      cellSize <- nchar(footer[col])
      padding <- colSize - cellSize
      startPadding <- padding
      endPadding <- 0
      cat(rep(space_symbol, startPadding), footer[[col]], rep(space_symbol, endPadding), sep = "")
    }
    cat("\n")
  }
  
  # Table Bottom ------------------------------------------------------------
  if (print_table_symbol == T) {
    cat(rep(table_symbol, maxLength + 2), "\n", sep = "")
  }
}

print_markdown_helper <- function(df) {
  
  
  # Set constants
  nme <- attr(df, "title", exact = T)
  decimal_mark <- getOption("OutDec", default = ".")
  decimal_digits <- getOption("simplefreqs.decimal_digits", default = 1)
  big_mark <- getOption("simplefreqs.big_mark", default = ",")
  
  # Replace NA with <NA> for printing
  # We check if <NA> alrady exist in data.frame and issues warning if it does
  lab <- levels(df[[1]])
  if ("<NA>" %in% lab) {
    warning('the string "<NA>" was detected. This conflicts with the printed NA results')
  }
  lab[is.na(lab)] <- "<NA>"
  levels(df[[1]]) <- lab
  
  # Determine if freqs are all integer or not (due to weighting)
  allInteger <- checkIfInteger(df[[2]])
  
  # Built GT table
  x <- df %>%
    gt(rowname_col = nme) %>%
    tab_stubhead(label = nme) 
  
  # Align columns
  x <- cols_align(x, align = "right", columns = c(1)) 
  
  # Format freq columns as integers if all integers
  if (allInteger == TRUE) {
    x <- fmt_number(x, columns = c(2, 4), sep_mark = big_mark, dec_mark = decimal_mark, decimals = 0)
  } else {
    x <- fmt_number(x, columns = c(2, 4), sep_mark = big_mark, dec_mark = decimal_mark, decimals = decimal_digits)
  }
  
  # Format percents
  x<- fmt_number(x, columns = c(3, 5), scale_by = 100, dec_mark = decimal_mark, decimals = decimal_digits)
  
  # Add total row
  print_total_row <- getOption("simplefreqs.print_table_total_row", default = TRUE)
  if (print_total_row==TRUE){
    # Format freq column as integers if all integers
    if (allInteger == TRUE) {
      x <- grand_summary_rows(x,columns = c(2,3), missing_text = "",
                              fns = list (Total = ~ sum(.)),
                              fmt = list(
                                ~ fmt_number(., columns = c("Freq"), dec_mark=decimal_mark, sep_mark=big_mark, decimals=0),
                                ~ fmt_percent(., columns = c("%"), decimals=0)
                              )
      )
    } else {
      x <- grand_summary_rows(x,columns = c(2,3), missing_text = "",
                              fns = list (Total = ~ sum(.)),
                              fmt = list(
                                ~ fmt_number(., columns = c("Freq"), dec_mark=decimal_mark, sep_mark=big_mark, decimals=decimal_digits),
                                ~ fmt_percent(., columns = c("%"), decimals=0)
                              )
      )
    }
    x <-  tab_style(x,
                    style = cell_text(align = "right"),
                    locations = cells_stub_grand_summary()
    )
  }
  table.width = getOption("simplefreqs.table.width", default=50)
  # Format table using options
  x <- tab_options(x, table.width = pct(table.width),
                   stub.border.style=NULL, 
                   stub.border.width=0,
                   table.border.top.style = "double",
                   table.border.top.width = "4pt",
                   table.border.top.color = "darkgray",
                   table.border.bottom.style = "double",
                   table.border.bottom.width = "4pt",
                   table.border.bottom.color = "darkgray",
                   column_labels.border.bottom.style = "solid",
                   column_labels.border.bottom.width = "2pt",
                   column_labels.border.bottom.color = "darkgray",
                   grand_summary_row.border.style = "solid",
                   grand_summary_row.border.width = "2pt",
                   grand_summary_row.border.color = "darkgray",
                   data_row.padding.horizontal = "10pt",
                   grand_summary_row.padding.horizontal = "10pt",
                   column_labels.padding.horizontal = "10pt"
  )
  
  # Further table formatting using styles
  x <- tab_style(x,
                 style = cell_text(align = "right"),
                 locations = cells_stubhead()
  )
  
  x <- tab_style(x,
                 style = cell_borders(weight = px(0)),
                 locations = cells_body()
  )
  
  x <- tab_style(x,
                 style = cell_borders(weight = px(0)),
                 locations = cells_stub()
  )
  
  return(x)
}



#' @export

print.simplefreqs_freq <- function(x, ...) {
  # Rename columns for printing
  names(x) <- c(attr(x, "title", exact = T), "Freq", "%", "Cum. Freq", "Cum. %")
  
  # Plot results
  plot <- attr(x, "plotted", exact = T)
  if (plot == TRUE) {
    nme <- attr(x, "title", exact = T)
    gg <- ggplot(data = x, aes(.data[[nme]], .data$Freq))
    gg <- gg + geom_bar(stat = "identity")
    gg <- gg + theme_minimal() + ggtitle(paste("Frequency:", nme)) + ylab("Count")
    gg <- gg + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(gg)
  }
  
  # Print table
  markdown <- attr(x, "markdown", exact = T)
  if (markdown == FALSE) {
    print_console_helper(x)
  } else {
    y<- print_markdown_helper(x)
    fun <- utils::getFromNamespace("knit_print.gt_tbl", "gt")
    return(fun(y))
    #return(gt:::knit_print.gt_tbl(y)) # ::: is not allowed on CRAN
  }
  
  
}
