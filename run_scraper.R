# Master script to run the scraper for the Mexico tariff site

# Libraries ---------------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(rvest)
library(netstat) # for free port function
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
               port = free_port(),
               chromever="94.0.4606.61",
               version = "3.141.59",
               extraCapabilities = eCaps_notimeout,
               check = FALSE)

# Assigns browser client to an object, remDr
remDr <- rD[["client"]]

# max waiting time to find element before throwing error (1 second)
#remDr$setTimeout(type = "implicit", milliseconds = 1000)

# how long to wait until page times out (2 mins)
remDr$setTimeout(type = "page load", milliseconds = 120000)

# Navigating to the URL
remDr$navigate(url)

# Run scraper - come back to 29
# TryCatch for page timeouts
# while(TRUE){
#   tryCatch({
#       for (i in c(63:97)) {
#         run_scrape(i, sleep_time = 4) %>% suppressMessages()
#         beepr::beep(3)
#         Sys.sleep(5)
#       }},
#       error = function(e){
#         print("Scraping restarted")
#         return(NULL)})
# }

# Try Catch for page load timeouts
selected_code <<- 96011001
while(TRUE){
    tryCatch({restart_scrape(selected_code)},
    error = function(e){
      print("Scraping restarted")
      return(NULL)
    })
}

# sample_df <- read_csv("data/hs44_data.csv", col_types = cols(.default = "c")) %>% 
#   filter(substr(hs_code, 1, 2) == "29")
# 
# sample_df %>% write_csv("data/hs29_data.csv")

