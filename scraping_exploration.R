# Packages ----------------------------------------------------------------
library(RSelenium)


rD <- rsDriver(browser = "chrome",
               port = 4120L,
               chromever="94.0.4606.61",
               version = "3.141.59",
               check = FALSE)

binman::list_versions("seleniumserver")
driver <- rsDriver(browser=c("chrome"), chromever="94.0.4606.61")

library(wdman)
selServ <- wdman::selenium(verbose = FALSE, port = 4444L)
selServ$log()
