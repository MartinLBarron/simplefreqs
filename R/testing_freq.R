#devtools::install_github("tidyverse/dplyr")
#Testing

# testing -----------------------------------------------------------------

library(forcats)
library(dplyr)
dfgss <-gss_cat

# Simple, single ------------------------------------------------------------------
freq(iris,Species)
freq(dfgss, year)
freq(dfgss$year)
freq(dfgss[,"year"], sort=F)
x <-freq(iris,Species)
print(x)
x

# # Missings ----------------------------------------------------------------

dfgss$year[c(seq(1,1000, by=4))] <- NA

freq(dfgss, year, na.rm=T)
x <- freq(dfgss, year, na.rm = T)
x

freq(dfgss, year, sort=F, na.rm = F)


# Multiple variables ------------------------------------------------------
freq(dfgss, year, marital, partyid)
#No longer working
# # # whole dataset -----------------------------------------------------------
# dfgss1 <- select(dfgss, year, marital, race, partyid)
# freq(dfgss1)
#
# # # save results ------------------------------------------------------------
# test2 <- freq(dfgss, year, saveResults = T)
# test3 <- freq(dfgss, year, saveResults = F)

#
# # sort --------------------------------------------------------------------
# freq(dfgss, year, sortResults = F)
# freq(dfgss, year, sortResults = T)
#
# # print Results -----------------------------------------------------------
# freq(dfgss, year, printResults = F)
# freq(dfgss, year, printResults = T)
#
# # plot Results -----------------------------------------------------------
# freq(dfgss, year, plotResults = F)
# freq(dfgss, year, plotResults = T)
#
#
# # Level warnings ----------------------------------------------------------
# df <- freq(dfgss, denom, levelError = T)
# df <- freq(dfgss, denom, levelError = F, sortResults = T, saveResults = T)
#

# # As part of a dplyr chain ------------------------------------------------
#As part of chain
# df <- dfgss %>%
#   filter(year>2006) %>%
#   freq(year)

