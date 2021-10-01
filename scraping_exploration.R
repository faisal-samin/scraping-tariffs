# Packages ----------------------------------------------------------------
library(RSelenium)
library(tidyverse)
library(rvest)


# Set up scraper ----------------------------------------------------------

# Debugging code
# binman::list_versions("seleniumserver")
# driver <- rsDriver(browser=c("chrome"), chromever="94.0.4606.61")
# 
# library(wdman)
# selServ <- wdman::selenium(verbose = FALSE, port = 4444L)
# selServ$log()

# Website to scrape
url = "http://www.economia-snci.gob.mx/"

rD <- rsDriver(browser = "chrome",
               port = 4125L,
               chromever="94.0.4606.61",
               version = "3.141.59",
               check = FALSE)

# Assigns browser client to an object, remDr
remDr <- rD[["client"]]

# Run to close driver
remDr$close()
rD$server$stop()
rm(rD)

# Navigating to the URL
remDr$navigate(url)


# Exploring the website (HS01) ---------------------------------------------------

# First thing to note is that the website is extremely slow and takes ages to load (upto 2-3 mins)
# There are four levels of hierarchy: HS2, HS4, HS6 and HS8.
# Clicking each value updates the values in the other lists. 

# An initial plan of action: extract all data under HS = "01" first

# Pseudocode
# for each HS2 code
# save list of HS4 to an array
# for each HS4 code
# save list of HS6 to an array
# for each HS6 code
# save list of HS8 if available otherwise continue (build in error handling)

# get list of HS4

# Save HTML of current web-page
html <- remDr$getPageSource()[[1]] %>% 
  read_html()

# get list of HS2 codes
html %>%
  html_node("frameset") %>% 
  html_nodes("frame") %>% 
  .[[2]]

# We're not seeing any results here as they are being called by ajax
# The principal iframe mentions /siavi5/fraccion.php from which the results are being called (with Ajax?)
# We could load in this html directly instead

url_principal <- "http://www.economia-snci.gob.mx/siavi5/fraccion.php"

# Assigns browser client to an object, remDr
remDr <- rD[["client"]]

# Navigating to the URL
remDr$navigate(url_principal)

html <- remDr$getPageSource()[[1]] %>% 
  read_html()

# This now returns more text!

# We can use the full xpath to grab the HS codes
hs4 <- html %>% 
  html_node(xpath = "/html/body/section/div[2]/div/form/table/tbody/tr/td[3]/table/tbody/tr[1]/td[4]/select") %>% 
  html_nodes("option") %>% 
  html_text()
  
# For each of these values, we want to select a HS4 option and then grab the resultant HS6 codes
hs6 <- html %>% 
  html_node(xpath = "/html/body/section/div[2]/div/form/table/tbody/tr/td[3]/table/tbody/tr[1]/td[6]/select") %>% 
  html_nodes("option") %>% 
  html_text()

# Likewise, with hs8
hs8 <- html %>% 
  html_node(xpath = "/html/body/section/div[2]/div/form/table/tbody/tr/td[3]/table/tbody/tr[1]/td[8]/select") %>% 
  html_nodes("option") %>% 
  html_text()

# To navigate across the codes, we may want to capture the locations
elem_hs <- remDr$findElements(using = "xpath", '//*[@id="idfra"]/option[2]') # select from hs8 list
#elems_hs <- remDr$findElements(using = "xpath", '//*[@id="idfra"]') # get list
sapply(elem_hs, function(x) x$getElementText())

# store selected_code
selected_code <- elem_hs[[1]]$getElementText()[[1]]

remDr$mouseMoveToLocation(webElement = elem_hs[[1]]) # move mouse to the element we selected

# and then click
remDr$click(1)

# Once we select a hs8 value, we want to select 2019, 2020 and 2021

# We can grab all the years by using their class name 
# (note that once a year is selected it no longer belongs to this class). But if the locations are stored
# before the years are clicked, it should be fine.

elem_year <- remDr$findElements("class name", "linksmensuales")
sapply(elem_year, function(x) x$getElementText()) # to print out the contents to verify

# store selected year
selected_year <- elem_year[[1]]$getElementText()[[1]]

# Store location of 2021 on the web-page for clicking
#loc <- elem_year[[1]]$getElementLocation()

remDr$mouseMoveToLocation(webElement = elem_year[[1]]) # move mouse to the element we selected

# and then click
remDr$click(1)

# We can now extract the four tables
# 1) Exports - valor

exports_valor <- remDr$getPageSource()[[1]] %>% # re-read the page source as it's changed
  read_html() %>%
  html_node(xpath = "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[4]") %>% 
  html_table()

# 2) Exports - volume
exports_volume <- remDr$getPageSource()[[1]] %>% # re-read the page source as it's changed
  read_html() %>%
  html_node(xpath = "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[6]") %>% 
  html_table()

# 3) Imports - valor
imports_valor <- remDr$getPageSource()[[1]] %>% # re-read the page source as it's changed
  read_html() %>%
  html_node(xpath = "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[8]") %>% 
  html_table()

# 4) Imports - volume
imports_volume <- remDr$getPageSource()[[1]] %>% # re-read the page source as it's changed
  read_html() %>%
  html_node(xpath = "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[10]") %>% 
  html_table()

# Note: 020120 for 2020 has a lot of detail in the table - broken down by country
# Once that's done we just need to combine the data into one dataframe

# Switching to 020120 manually to build df code
# and then re-run the four dataframes

# To bind the dataframes, there are four steps:
# 1) set empty df with col headers covering hs code, year, header + 12 months (14 cols)
# 2) select only first 13 columns (header + 12 months). Even 2021 results will have 39 columns but col headers are blank
# 3) for each df, in first 2 cols, enter hs code and year
# 4) bind to master df

# 1
df_master <- tibble::tibble(
  hs_code = character(),
  year = character(),
  País = character(),
  Enero = character(),
  Febrero = character(),
  Marzo = character(),
  Abril = character(),
  Mayo = character(),
  Junio = character(),
  Julio = character(),
  Agosto = character(),
  Septiembre = character(),
  Octubre = character(),
  Noviembre = character(),
  Diciembre = character()
)

# 2 select, bind and add to master df

df_loop <-  bind_rows(exports_valor[,1:13],
                       exports_volume[,1:13],
                       imports_valor[,1:13],
                       imports_volume[,1:13]) %>% 
  mutate(hs_code = selected_code,
         year = selected_year)

df_master <- bind_rows(df_master, df_loop)
