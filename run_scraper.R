# Master script to run the scraper for the Mexico tariff site

# Libraries ---------------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(rvest)
library(tictoc)
source("functions.R") # source in code for scraper

# Scraper -----------------------------------------------------------------

# Website to scrape
url = "http://www.economia-snci.gob.mx/siavi5/fraccion.php"

# Selenium driver kept outside of main function for quick config of ports and browserver
rD <- rsDriver(browser = "chrome",
               port = 4120L,
               chromever="94.0.4606.61",
               version = "3.141.59",
               check = FALSE)

# Assigns browser client to an object, remDr
remDr <- rD[["client"]]

# Run if needed to reset driver and browser client
cleanup()

# Navigating to the URL
remDr$navigate(url)

# Run scraper
tic()
run_scrape(hs2_code = 2, sleep_time = 7) #hs98
toc()
