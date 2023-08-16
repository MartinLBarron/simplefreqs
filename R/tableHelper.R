# Custom helpers and print functions for classes

# Helper functions --------------------------------------------------------


# Determine number of decimals --------------------------------------------
decimalplaces <- function(x) {
  if (!is.na(x)) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub("0+$", "", as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
}

# Determine Max width of columns ------------------------------------------
DetermineColumnWidth <- function(x) {
  # x <- as.character(x)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- max(sapply(x, nchar), na.rm = T)
  return(x)
}

# Given a vector, tries to guess best formatting
formatGuesser <- function(x) {
  if (min(x, na.rm = T) >= 100) {
    f <- "n0"
  } else if (max(unlist(sapply(x, decimalplaces)), na.rm = T) == 0) {
    f <- "n0"
  } else if (min(unlist(x), na.rm = T) >= 10) {
    f <- "n1"
  } else if (max(unlist(sapply(x, decimalplaces)), na.rm = T) == 1) {
    f <- "n1"
  } else {
    f <- "n1"
  }
  return(f)
}

# given a vector and a string indicating , r
formatColumn <- function(x, f = NA) {
  # percent0
  if (f == "c") {
    x <- as.character(x)
  } else if (f == "p0") {
    x <- paste0(formatC(100 * x, digits = 0, format = "f"), "%")
    
    # percent1
  } else if (f == "p1") {
    x <- paste0(formatC(100 * x, digits = 1, format = "f"), "%")
    
    # percent3
  } else if (f == "p2") {
    x <- paste0(formatC(100 * x, digits = 2, format = "f"), "%")
    
    # percent3
  } else if (f == "p3") {
    x <- paste0(formatC(100 * x, digits = 3, format = "f"), "%")
    
    # numeric0
  } else if (f == "n0") {
    x <- formatC(x, format = "f", digits = 0, big.mark = ",")
    
    # numeric1
  } else if (f == "n1") {
    x <- formatC(x, format = "f", digits = 1, big.mark = ",")
    
    # numeric2
  } else if (f == "n2") {
    x <- formatC(x, format = "f", digits = 2, big.mark = ",")
  } else if (f == "asis") {
    x <- x
  }
  
  return(x)
}
# x <- as.data.frame(apply(df,2, formatColumn))



# Determine width of all columns ------------------------------------------

DetermineColumnWidths <- function(df) {
  # Determine width of data
  data <- sapply(df, DetermineColumnWidth)
  
  # Determine width of headers
  header <- names(df)
  header <- sapply(header, DetermineColumnWidth)
  
  results <- ifelse(header > data, header, data)
  return(results)
}

# x <- as.data.frame(apply(df,2, formatColumn), stringsAsFactors = F)

# \u2500 is a single linke
# \u2550 is a double line

# Generic function to lay out table as desired ---------------------------------
print_helper <- function(df,
                         formats = NA,
                         inner_table_padding = getOption("SimpleFreqs.inner_table_padding", default = 5),
                         row_divider_symbol = getOption("SimpleFreqs.row_divider_symbol", default = "\u2500"),
                         table_symbol = getOption("SimpleFreqs.table_symbol", default = "\u2550"), 
                         print_table_symbol = getOption("SimpleFreqs.print_table_symbol", default = TRUE), 
                         print_total_row = getOption("SimpleFreqs.print_table_total_row", default = TRUE), 
                         print_metadata = getOption("SimpleFreqs.print_table_metadata", default = TRUE), 
                         print_header_divider = getOption("SimpleFreqs.print_header_divider", default = TRUE)){
  
  space_symbol = " "
  # Get total N
  n <- sum(df$Freq)
  
  # Replace NA with <NA> for printing
  # We check if <NA> exist in dataset and issues warning if it does
  lab <- levels(df[[1]])
  if ("<NA>" %in% lab){
    warning ('the string "<NA>" was detected. This conflicts with the printed NA results')
  }
  lab[is.na(lab)] <- "<NA>"
  levels(df[[1]]) <- lab
  
  
  
  # Convert Dataframe to all character
  # df <- as.data.frame(lapply(df, formatColumn), stringsAsFactors = F)
  # format as specified
  if (length(formats) == 1) {
    if (is.na(formats)) {
      formats <- rep("c", length(df))
    }
  }
  for (i in 1:length(df)) {
    df[i] <- formatColumn(df[[i]], f = formats[i])
  }
  
  # Get Window Width
  windowWidth <- getOption("width")
  
  # Get number of columns and width
  totalColumns <- length(df)
  maxColWidth <- DetermineColumnWidths(df)
  # add margins to columns
  maxColWidth <- maxColWidth + inner_table_padding
  
  # Determine number of columns that can fit on screen
  maxColWidthRunning <- cumsum(maxColWidth)
  maxColWidthRunning <- ifelse(maxColWidthRunning > windowWidth, F, T)
  maxCols <- sum(maxColWidthRunning)
  maxLength <- sum(maxColWidth)
  
  # get column names
  nme <- names(df)
  # get column %s
  totals <- c("Total", n, "100%", n, "100%")
  
  # Print Metadata ---------------------------------------------------------
  if (print_metadata == TRUE) {
    cat("\nFREQUENCY: ", attr(df, "title"), "\n", sep = "")
    cat("Class: ", attr(df, "varClass", exact=T), "\n", sep = "")
    cat("Type: ", attr(df, "varType", exact=T), "\n", sep = "")
    cat("Mode: ", attr(df, "varMode", exact=T), "\n", sep = "")
    cat("Missing: ", attr(df, "missing", exact=T), "\n", sep = "")
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
  
  if (print_header_divider==TRUE){
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
      cellSize <- nchar(totals[col])
      padding <- colSize - cellSize
      startPadding <- padding
      endPadding <- 0
      cat(rep(space_symbol, startPadding), totals[[col]], rep(space_symbol, endPadding), sep = "")
    }
    cat("\n")
  }
  
  # Table Bottom ------------------------------------------------------------
  if (print_table_symbol == T) {
    cat(rep(table_symbol, maxLength + 2), "\n", sep = "")
  }
}

#' S3 Method to print SimpleFreqs data
#' @export

print.SimpleFreqs_freq <- function(x, ...) {
  names(x) <- c(attr(x, "title", exact = T), "Freq", "%", "Cum. Freq", "Cum. %")
  
  print_helper(x, formats = c("c", "n0", "n1", "n0", "n1"))
  
  missing <- attr(x, "MissingRemoved", exact = T)
  if (!is.null(missing)) {
    norig <- sum(x$Freq) + missing
    naPercent <- (missing / norig) * 100
    cat(paste0(attr(x, "title", exact = T), " NA's excluded: ", prettyNum(missing, big.mark = ","), " (", formatC(naPercent, digits = 1, format = "f"), "%)\n\n"))
  }
}
