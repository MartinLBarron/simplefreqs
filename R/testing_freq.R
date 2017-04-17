# #devtools::install_github("tidyverse/dplyr")
# #Testing
# 
# # testing -----------------------------------------------------------------
# 
# library(forcats)
# library(dplyr)
# dfgss <-gss_cat
# 
# 
# # Simple ------------------------------------------------------------------
# df <-freq(dfgss, year)
# 
# 
# 
# # save results ------------------------------------------------------------
# df <- freq(dfgss, year, saveResults = T)
# 
# # sort --------------------------------------------------------------------
# df <- freq(dfgss, year, sortResults = F)
# 
# 
# # print Results -----------------------------------------------------------
# 
# df <- freq(dfgss, year, printResults = F)
# 
# 
# # Level warnings ----------------------------------------------------------
# df <- freq(dfgss, denom)
# df <- freq(dfgss, denom, levelWarning = F)
# 
# 
# # Missings ----------------------------------------------------------------
# 
# dfgss$year[c(seq(1,1000, by=4))] <- NA
# 
# x <-freq(dfgss, year)
# x <-freq(dfgss, denom)
# 
# x <-freq(dfgss, year, use.NA = F, levelWarning=F)
# 
# 
# # # As part of a dplyr chain ------------------------------------------------
# #
# #
# #As part of chain
# df <- dfgss %>%
#   filter(year>2006) %>%
#   freq(year)
# 
