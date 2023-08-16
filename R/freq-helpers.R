# helpers and print functions for class

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
print_helper <- function(df,
                         inner_table_padding = getOption("SimpleFreqs.inner_table_padding", default = 5),
                         row_divider_symbol = getOption("SimpleFreqs.row_divider_symbol", default = "\u2500"),
                         table_symbol = getOption("SimpleFreqs.table_symbol", default = "\u2550"),
                         print_table_symbol = getOption("SimpleFreqs.print_table_symbol", default = TRUE),
                         print_total_row = getOption("SimpleFreqs.print_table_total_row", default = TRUE),
                         print_metadata = getOption("SimpleFreqs.print_table_metadata", default = TRUE),
                         print_header_divider = getOption("SimpleFreqs.print_header_divider", default = TRUE)) {
  # Set Constants
  space_symbol <- " "
  big_mark <- getOption("SimpleFreqs.big_mark", default = ",")
  n <- sum(df$Freq)

  # Replace NA with <NA> for printing
  # We check if <NA> alrady exist in data fram and issues warning if it does
  lab <- levels(df[[1]])
  if ("<NA>" %in% lab) {
    warning('the string "<NA>" was detected. This conflicts with the printed NA results')
  }
  lab[is.na(lab)] <- "<NA>"
  levels(df[[1]]) <- lab


  missing <- attr(df, "missing", exact = T)
  missingRemoved <- attr(df, "missing_removed", exact = T)

  if (!is.null(missingRemoved)) {
    norig <- sum(df$Freq) + missing
  } else {
    norig <- sum(df$Freq)
  }
  naPercent <- (missing / norig) * 100



  # Convert Dataframe to all character
  # format as specified
  df[1] <- as.character(df[[1]])
  df[2] <- formatC(df[[2]], format = "f", digits = 0, big.mark = big_mark)
  df[3] <- formatC(df[[3]] * 100, format = "f", digits = 1)
  df[4] <- formatC(df[[4]], format = "f", digits = 0, big.mark = big_mark)
  df[5] <- formatC(df[[5]] * 100, format = "f", digits = 1)

  # Format Footer
  footer <- c(
    "Total",
    formatC(n, format = "f", digits = 0, big.mark = big_mark),
    "100%",
    formatC(n, format = "f", digits = 0, big.mark = big_mark),
    "100%"
  )
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
      cat(paste0("NA's removed: ", prettyNum(missing, big.mark = big_mark), " (", formatC(naPercent, digits = 1, format = "f"), "%)\n"))
    } else {
      cat(paste0("NA's: ", prettyNum(missing, big.mark = big_mark), " (", formatC(naPercent, digits = 1, format = "f"), "%)\n"))
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

#' @export

print.SimpleFreqs_freq <- function(x, ...) {
  # Rename columns for printing
  names(x) <- c(attr(x, "title", exact = T), "Freq", "%", "Cum. Freq", "Cum. %")

  # Print table
  print_helper(x)
}