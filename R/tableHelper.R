#Custom helpers and print functions for classes

# Helper functions --------------------------------------------------------


# Determine number of decimals --------------------------------------------

decimalplaces <- function(x) {
  if (!is.na(x)){
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
}

# Determine Max width of columns ------------------------------------------
DetermineColumnWidth <- function(x){
  #x <- as.character(x)
  if (is.factor(x)){
    x <- as.character(x)
  }
  x <- max(sapply(x, nchar),na.rm = T)
  return(x)

}

#Given a vector, tries to guess best formatting
formatGuesser <- function(x){
  if (min(x, na.rm=T)>=100){
    f="n0"
  }else if (max(unlist(sapply(x,decimalplaces)), na.rm=T)==0){
    f="n0"
  }else if (min(unlist(x), na.rm=T)>=10){
    f="n1"
  } else if (max(unlist(sapply(x,decimalplaces)), na.rm=T)==1){
    f="n1"
  } else {
    f="n1"
  }
  return(f)
}

#given a vector and a string indicating , r
formatColumn <- function(x, f=NA){
  print("formatColumn")

  if(!(is.character(x) | is.factor(x))){
    print("guess")
    print(x)
    if (is.na(f)){
      f <-formatGuesser(x)
    }
    if (f=="g"){
      f <-formatGuesser(x)
    }
    #percent0
    if (f=="p0") {
      x <- paste0(formatC(100 * x, digits=0, format="f"), "%")

      #percent1
    } else if (f=="p1"){
      x <- paste0(formatC(100 * x, digits=1, format="f"), "%")

      #percent3
    } else if (f=="p2"){
      x <- paste0(formatC(100 * x, digits=2, format="f"), "%")

      #percent3
    } else if (f=="p3"){
      x <- paste0(formatC(100 * x, digits=3, format="f"), "%")


      #numeric0
    } else if (f=="n0"){
      x <- formatC(x, format="f", digits=0, big.mark = ",")

      #numeric1
    } else if (f=="n1"){
      x <- formatC(x, format="f", digits=1, big.mark = ",")

      #numeric2
    } else if (f=="n2"){
      x <- formatC(x, format="f", digits=2, big.mark = ",")
    } else if (f=="asis"){
      x <- x
    }
  }
  if (is.factor(x)){
    x <- as.character(x)
  }
  return(x)
}
# x <- as.data.frame(apply(df,2, formatColumn))



# Determine width of all columns ------------------------------------------

DetermineColumnWidths <- function(df){

  #Determine width of data
  data <-sapply(df,DetermineColumnWidth)

  #Determine width of headers
  header <-names(df)
  header <- sapply(header, DetermineColumnWidth)

  results<-ifelse(header>data,header,data)
  return(results)


}

# x <- as.data.frame(apply(df,2, formatColumn), stringsAsFactors = F)

# Generic function to lay out table as desired ---------------------------------
printIt <- function(df, breaks=NA, formats=NA, margin=5, divider="", upperSymbol="=", lowerSymbol="-", center=F, tableSymbol="_", tablePadding=5, spaceSymbol=" ", printTableSymbol=T, printHeaderRow=T, printTotalRow=T){

  #Convert Dataframe to all character
  df <- as.data.frame(lapply(df, formatColumn), stringsAsFactors = F)
  #format as specified

  #Get Window Width
  windowWidth <- getOption("width")

  #Get number of columns and width
  totalColumns <- length(df)
  maxColWidth <- DetermineColumnWidths(df)
  #add margins to columns
  maxColWidth <- maxColWidth + margin

  #Determine number of columns that can fit on screen
  maxColWidthRunning <- cumsum(maxColWidth)
  maxColWidthRunning <- ifelse(maxColWidthRunning>windowWidth,F,T)
  maxCols <- sum(maxColWidthRunning)
  maxLength <- sum(maxColWidth)

  #get column names
  nme <-names(df)


  # Print Table top ---------------------------------------------------------
  #print outer
  if (printTableSymbol==T){
    cat(rep(tableSymbol, maxLength+(tablePadding*2)),"\n",sep="")
  }

  # Print Header ------------------------------------------------------------
  #padding for header labels
  cat(rep(spaceSymbol, tablePadding), sep="")

  for (col in 1:length(df)){
    colSize=maxColWidth[col]
    #determine padding
    cellSize=nchar(nme[col])
    padding=colSize-cellSize
    if (center==T){
      startPadding = floor(padding/2)
      endPadding= padding-startPadding
    } else{
      startPadding=padding
      endPadding=0
    }
    cat(rep(spaceSymbol, startPadding), nme[[col]], rep(spaceSymbol, endPadding),divider, sep="")
  }
  cat("\n")
  if (printHeaderRow==T){
    cat(rep(spaceSymbol, tablePadding-1),rep(upperSymbol, maxLength+2),"\n",sep="")
  }

  #Now print cell values
  for (row in 1:nrow(df)) {
    #print break rows when requested
    #determine if breakrow
    if (row %in% breaks){
      cat(rep(spaceSymbol, tablePadding-1),rep(lowerSymbol, maxLength+2),"\n",sep="")
    }

    cat(rep(spaceSymbol, tablePadding), sep="")
    for (col in 1:length(df)){
      colSize=maxColWidth[col]
      #determine padding
      if(is.na(df[row,col])){
        cellSize=2
      }else{
        cellSize=nchar(df[row,col])
      }
      padding=colSize-cellSize
      cat(rep(spaceSymbol, padding), df[[row,col]], divider, sep="")
    }
    cat("\n")
  }

  #print outer

  if (printTotalRow==T){
    cat(rep(spaceSymbol, tablePadding-1),rep(upperSymbol, maxLength+2),"\n",sep="")
  }
  # Table Bottom ------------------------------------------------------------
  if (printTableSymbol==T){
    cat(rep(tableSymbol, maxLength+(tablePadding*2)),"\n",sep="")
  }
}


print.freqR_summaryMeans <-function(df){
  breaks <- c(4,7)
  printIt(df, breaks, printTotalRow = F)
}

print.freqR_freq <-function(df){
  breaks <- NA
  printIt(df, breaks, formats=c("g", "n0","p2", "n2", "p2"), printTotalRow = T)
}

print.freqR_listFreq <-function(df){
  breaks <- NA
  print("printing listfreq")
  printIt(df, breaks, formats=c("g", "n0","p2", "n0", "p2"), printTotalRow = T)
}


# breaks <- NA
# printIt(df, breaks)


#
# max(sapply(df$wt,decimalplaces))
#

# x<-df[[2]]
# formatter(df[[3]])

