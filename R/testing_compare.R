testing <- function(){
  library(freqR)
  library(dplyr)
  rm(list=ls())
  #create a dataset for testing
  df <- mtcars
  
  #create some missing variables
  df[1,1]<-NA
  df[5,1]<-NA
  df[2,2]<-NA
  
  
  # Test Summary Means ------------------------------------------------------
  test <- compare(df, mpg, cyl, disp)
  test
  test <- compare(df, mpg, cyl, disp, na.rm=T, long = T)
  test
  
  test <- compare(df, mpg, group_var = cyl, long=T)
  test
  
  test2 <-tidyr::spread(test, key=type, value=mpg)
  
}

