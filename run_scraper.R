# Master script to run the scraper for the Mexico tariff site

# Libraries ---------------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(rvest)
library(tictoc)
library(beepr)
source("functions.R") # source in code for scraper

# Scraper -----------------------------------------------------------------

# Website to scrape
url = "http://www.economia-snci.gob.mx/siavi5/fraccion.php"

# Chrome prefs and args for scraper
chrome_prefs = 
  list(  
    # chrome prefs
    "profile.default_content_settings.popups" = 0L,
    "download.prompt_for_download" = FALSE)

chrome_args = list(
  '--window-size=1200,1800')
    # chrome command arguments
    #'--headless',
    #'--window-size=1200,1800')
    #'--sessionTimeout 57868143'

eCaps_notimeout = 
  list(chromeOptions = 
         list(
           prefs = chrome_prefs,
           args = chrome_args 
         ))

# Selenium driver kept outside of main function for quick config of ports and browserver
rD <- rsDriver(browser = "chrome",
               port = 4360L,
               chromever="94.0.4606.61",
               version = "3.141.59",
               extraCapabilities = eCaps_notimeout,
               check = FALSE)

# Assigns browser client to an object, remDr
remDr <- rD[["client"]]

# max waiting time to find element before throwing error (1 second)
#remDr$setTimeout(type = "implicit", milliseconds = 1000)

# how long to wait until page times out (20 mins)
remDr$setTimeout(type = "page load", milliseconds = 12000000)

# Run if needed to reset driver and browser client
remDr$close()
rD$server$stop()
rm(rD)

# Navigating to the URL
remDr$navigate(url)

# Run scraper (29 remaining)
for (i in c(29)) {
  tic()
  run_scrape(hs2_code = i, hs4_code_start = 15, sleep_time = 4) %>% suppressMessages()
  toc()
  beepr::beep(3)
  Sys.sleep(5)
}

# hs3 incomplete, complete rest when function developed