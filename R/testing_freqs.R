# #Testing
# 
# # testing -----------------------------------------------------------------
# 
# library(forcats)
# library(dplyr)
# dfgss <-gss_cat
# 
# # Simple, single ------------------------------------------------------------------
# freqs(iris,Species, Sepal.Width)
# freqs(iris)
# # # Missings ----------------------------------------------------------------
# 
# dfgss$year[c(seq(1,1000, by=4))] <- NA
# dfgss$age[c(seq(1,1000, by=4))] <- NA
# 
# freqs(dfgss, year, age, na.rm=T)
# 
# # # sort --------------------------------------------------------------------
# freqs(dfgss, year, age, sort = F)
# freqs(dfgss, year, age, sort = T)
# 
# 
# # # # plot Results -----------------------------------------------------------
# freqs(dfgss, year, age, plot = F)
# freqs(dfgss, year, age, plot = T)
# 
