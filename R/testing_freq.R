# #devtools::install_github("tidyverse/dplyr")
# #Testing
# 
# # testing -----------------------------------------------------------------
# 
# library(forcats)
# library(dplyr)
# dfgss <-gss_cat
# dfgss <- select(dfgss, -tvhours, -age, -denom)
# dfgss <- freq2(dfgss, saveResults = T)
# 
# print(dfgss)
# 
# 
# 
# # Simple ------------------------------------------------------------------
# freq(iris,Species)
# freq(dfgss, year)
# # save results ------------------------------------------------------------
# freq(dfgss, year, saveResults = T)
# freq(dfgss, year, saveResults = F)
# 
# 
# # sort --------------------------------------------------------------------
# df <- freq(dfgss, year, sortResults = F)
# df <- freq(dfgss, year, sortResults = T)
# 
# # print Results -----------------------------------------------------------
# df <- freq(dfgss, year, printResults = F)
# df <- freq(dfgss, year, printResults = T)
# 
# 
# # Level warnings ----------------------------------------------------------
# df <- freq(dfgss, denom, levelError = T)
# df <- freq(dfgss, denom, levelError = F, sortResults = T, saveResults = T)
# 
# # Missings ----------------------------------------------------------------
# 
# dfgss$year[c(seq(1,1000, by=4))] <- NA
# 
# x <-freq(dfgss, year)
# x <-freq(dfgss, year, na.rm = T)
# 
# # As part of a dplyr chain ------------------------------------------------
# #As part of chain
# df <- dfgss %>%
#   filter(year>2006) %>%
#   freq(year)
# 
# 
# 
# 
# 
