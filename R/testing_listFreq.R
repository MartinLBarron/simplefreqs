# #devtools::install_github("tidyverse/dplyr")
# #Testing
# 
# # testing -----------------------------------------------------------------
# library(freqR)
# library(forcats)
# library(dplyr)
# dfgss <- gss_cat
# 
# # Simple, single ------------------------------------------------------------------
# listFreq(dfgss, year,race, marital, race)
# 
# # # save results ------------------------------------------------------------
# test2 <- listFreq(dfgss, year,race, marital, race, saveResults = T)
# test3 <- listFreq(dfgss, year,race, marital, race, saveResults = F)
# 
# # # sort --------------------------------------------------------------------
# test2 <- listFreq(dfgss, year,race, marital, race, sortResults = T)
# test3 <- listFreq(dfgss, year,race, marital, race, sortResults = F)
# 
# # # print Results -----------------------------------------------------------
# test2 <- listFreq(dfgss, year,race, marital, race, printResults = T)
# test3 <- listFreq(dfgss, year,race, marital, race, printResults = F)
# 
# # # plot Results -----------------------------------------------------------
# test2 <- listFreq(dfgss, year,race, marital, race, plotResults = T)
# test3 <- listFreq(dfgss, year,race, marital, race, plotResults = F)
# 
# # # Level warnings ----------------------------------------------------------
# test2 <- listFreq(dfgss, year,race, marital, race, denom, levelError = T)
# test3 <- listFreq(dfgss, year,race, marital, race, denom, levelError  = F, saveResults = T)
# 
# # # Missings ----------------------------------------------------------------
# #
# # dfgss$year[c(seq(1,1000, by=4))] <- NA
# #
# 
# # x <-freq(dfgss, year)
# # x <-freq(dfgss, year, na.rm = T)
# #
# # # As part of a dplyr chain ------------------------------------------------
# # #As part of chain
# # df <- dfgss %>%
# #   filter(year>2006) %>%
# #   freq(year)
# #
