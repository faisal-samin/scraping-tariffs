# Set of functions for different parts of the scraping pipeline

run_scrape <- function(hs_code, sleep_time) {
  # Inputs: hs_code, if longer than 2 digits, it will resume scrape from that code
  # Outputs: returns dataframe with scraped data
  
  # convert to string
  hs_str <- as.character(hs_code)

  # Navigating to the URL = this is to refresh the page to avoid issue with hs2 code
  remDr$navigate(url)
  
  # Refresh live html along with remDr elements of interest (hs code list, years)
  refresh_elements()
  
  # Select hs2 code and move mouse to hs2 code
  remDr$mouseMoveToLocation(webElement = elem_hs2[[as.integer(substr(hs_str, 1, 2))]])
  
  # pause until hs2 code is in view
  Sys.sleep(1)
  #click
  remDr$click(1)
  
  Sys.sleep(6)
  refresh_elements()
  
  # Initiate dataframes at each level if missing
  if(!exists("df_hs4")){df_hs4 <<- initiate_df()}
  if(!exists("df_hs6")){df_hs6 <<- initiate_df()}
  if(!exists("df_hs8")){df_hs8 <<- initiate_df()}
  
  # 1st loop - loop through HS4 codes
  # if the selected code is 2 digits, i.e. it's not a restart
  if(nchar(hs_str) == 2){
    # start index from 1
    hs4_code_start <- 1
  } else {
    # pick out index from input code
    hs4_code_start <- map_hs_index(substr(hs_str, 1, 4))
  }

  for (i in hs4_code_start:length(elem_hs4)){
    
    # move mouse to hs4 code
    remDr$mouseMoveToLocation(webElement = elem_hs4[[i]])
    #click
    remDr$click(1)
    
    Sys.sleep(6)
    refresh_elements()
    # if the selected code is 2 digits, i.e. it's not a restart
    if(nchar(hs_str) == 2){
      # start index from 1
      hs6_code_start <- 1
    } else {
      # pick out index from input code
      hs6_code_start <- map_hs_index(substr(hs_str, 1, 6))
    }
    
    # 2nd loop - loop through HS6 codes
    for (i in hs6_code_start:length(elem_hs6)) {
      
      # move mouse to hs6 code
      remDr$mouseMoveToLocation(webElement = elem_hs6[[i]])
      #click
      remDr$click(1)
      
      Sys.sleep(6)
      refresh_elements()
      
      # 3rd loop - loop through HS8 codes
      if(nchar(hs_str) == 2){
        # start index from 1
        hs8_code_start <- 1
      } else {
        # pick out index from input code
        hs8_code_start <- map_hs_index(substr(hs_str, 1, 8))
        # once the hs8 code has been scraped, we treat the rest of the code like normal, hence new hs_str
        hs_str <- substr(hs_str, 1, 2)
      }
      
      for (i in hs8_code_start:length(elem_hs8)) {
        # move mouse to hs8 code
        remDr$mouseMoveToLocation(webElement = elem_hs8[[i]])
        #click
        remDr$click(1)

        # sleep to wait for tables to load
        Sys.sleep(6)
        
        refresh_elements()
        
        # Store selected HS8 code
        selected_code <<- elem_hs8[[i]]$getElementText()[[1]]
        
        # 5th loop - loop through each year (we only want 2020 and 2019)
        df_year <<- initiate_df()
        for (i in 2:3) {
          # Try/Catch for error handling when gathering table data 
          # This is required as sometimes the tables can take very long to load (30 seconds)
          # Only required for first table, as, if it loads, the others should also load
          
          # move mouse to year
          remDr$mouseMoveToLocation(webElement = elem_year[[i]])
          # and then click
          
          # TO DO - build error handling around this click in the case of timeouts
          remDr$click(1)
          Sys.sleep(sleep_time)
          
          exports_valor <- NULL
          counter <- 0
          while(is.null(exports_valor)) {
            counter <- counter + 1
            print(counter)
            if(counter == 10){
              restart_scrape(selected_code)
              return()
            }
            
            exports_valor <- tryCatch({
              Sys.sleep(3)
              refresh_elements()
              exports_valor <- gather_table("Exports valor", "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[4]")},
              error = function(e){
                print("TryCatch triggered")
                return(NULL)
                })
          }
          
          exports_volume <- gather_table("Exports volume", "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[6]")
          imports_valor <- gather_table("Imports valor", "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[8]")
          imports_volume <- gather_table("Imports volume", "/html/body/section/section[2]/div/div/table/tbody/tr/td/div[2]/table[10]")
          
          # Store selected year
          selected_year <- elem_year[[i]]$getElementText()[[1]]
          
          df_year <<- update_df(df_year, 
                               exports_valor %>% mutate(across(everything(), as.character)), 
                               exports_volume %>% mutate(across(everything(), as.character)),
                               imports_valor %>% mutate(across(everything(), as.character)),
                               imports_volume %>% mutate(across(everything(), as.character)),
                               selected_code,
                               selected_year)
          
          print(paste0(selected_code, " - ", selected_year))
        } # end of 4th loop
        # bind year loop with hs8 df
        df_hs8 <<- bind_rows(df_hs8, df_year) %>% unique() # safeguard against duplicates
        } # end of 3rd loop
      df_hs6 <<- bind_rows(df_hs6, df_hs8)
      df_hs8 <<- initiate_df() # wipe clean for next iteration
    } # end of 2nd loop
    df_hs4 <<- bind_rows(df_hs4, df_hs6)
    df_hs6 <<- initiate_df()
  } # end of 1st loop
  df_hs4 %>% write_csv(paste0("data/hs", substr(hs_str, 1, 2), "_data.csv"))
  df_hs4 <<- initiate_df()
  rm(df_hs4, df_hs6, df_hs8) # required to start fresh with next loop
}

# df_hs4 %>% write_csv(paste0("data/hs", "29", "_data.csv"))

gather_hs_codes <- function(xpath_hs) {
  # gathers HS codes in the form of Selenium Elements at the given xpath
  # Inputs: driver (selenium driver, typically remDr); xpath for HS codes
  # Outputs: List of Selenium elements at that xpath
  # Issues: 
  
  # Possible options: 
  # HS2: "//*[@id="idcap"]/option" # note, hs2 code 77 code doesn't exist
  # HS4: "//*[@id="idpar"]/option"
  # HS6: "//*[@id="idsub"]/option"
  # HS8: "//*[@id="idfra"]/option"
  
  # select from hs8 list
  elem_hs <- remDr$findElements(using = "xpath", xpath_hs) 
  
  # Optional: to print elements
  # return(sapply(elem_hs, function(x) x$getElementText()))
  
  return(elem_hs)
}
gather_table <- function(table_name, xpath_table) {
  # Gathers a dataframe from the given xpath of the results show after selecting a year
  # Input: name of table to insert into df, xpath to table
  # Output: a dataframe with all elements in the table
  # Issues:
  
  tbl_name <- html %>% 
                html_node(xpath = xpath_table) %>% 
                html_table() %>% 
                as_tibble(.name_repair = "unique") %>%
                .[,1:13] %>% 
                mutate(name = table_name, .before = "País")
  
  names(tbl_name) <- html_col_names
  return(tbl_name)
}
initiate_df <- function() {
  # Set up master df with required months
  # Input: none
  # Output: empty df with named columns
  tibble::tibble(
    hs_code = character(),
    year = character(),
    name = character(),
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
}
update_df <- function(df_main, exports_val, exports_vol, imports_val, imports_vol,
                      selected_code, selected_year) {
  # Update master df with cuts from the four tables
  # Inputs: master df, and four df from current web page
  # Outputs: binded df
  # Issues:
  
  df_loop <- bind_rows(exports_val, 
                       exports_vol,
                       imports_val,
                       imports_vol) %>% 
    mutate(hs_code = selected_code,
           year = selected_year)
  
  return(bind_rows(df_main, df_loop))
}
cleanup <- function() {
  # Clear memory of selenium driver (rD), browser client (remDr), selenium server
  # Inputs: none
  # Outputs: none
  # Issues: assumes remDr and rD variables 
  out <- tryCatch({
      remDr$close()
      rD$server$stop()
      rm(rD)},
    error = function(e){return(NULL)})
  
  return(out)
}
refresh_elements <- function() {
  # Whenever the page is clicked/updated, all remDr elements need to be refreshed
  # This function achieves that for the relevant elements
  # Input: none
  # Output: none
  # Issues
  
  # Double arrow to update global variables
  remDr <<- rD[["client"]]
  html <<- remDr$getPageSource()[[1]] %>% read_html()
  elem_hs2 <<- gather_hs_codes('//*[@id="idcap"]/option')
  elem_hs4 <<- gather_hs_codes('//*[@id="idpar"]/option')
  elem_hs6 <<- gather_hs_codes('//*[@id="idsub"]/option')
  elem_hs8 <<- gather_hs_codes('//*[@id="idfra"]/option')
  elem_year <<- remDr$findElements(using = "xpath", '//*[@id="tabla"]/span[1]/a')
}
restart_scrape <- function(selected_code) {
  # invoked when more than 10 tryCatch errors occur 
  
  # close session
  cleanup()
  
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
  
  eCaps_notimeout = 
    list(chromeOptions = 
           list(
             prefs = chrome_prefs,
             args = chrome_args 
           ))
  
  # Selenium driver kept outside of main function for quick config of ports and browserver
  rD <<- rsDriver(browser = "chrome",
                 port = free_port(),
                 chromever="94.0.4606.61",
                 version = "3.141.59",
                 extraCapabilities = eCaps_notimeout,
                 check = FALSE)
  
  # Assigns browser client to an object, remDr
  remDr <<- rD[["client"]]
  
  # how long to wait until page times out (2 mins)
  remDr$setTimeout(type = "page load", milliseconds = 120000)
  
  Sys.sleep(2)
  
  # Navigating to the URL
  remDr$navigate(url)
  
  Sys.sleep(1)
  
  # Load in existing dataframe (if it exists)
  tryCatch({
    df_hs4 <<- read_csv(paste0("data/hs", substr(selected_code, 1, 2) , "_data.csv"),
                        col_types = cols(.default = "c"))},
    error = function(e){
      return(NULL)
    })
  
  # resume scrape for just that hs2 code
  tic()
  run_scrape(selected_code, sleep_time = 4) %>% suppressMessages()
  toc()
  beepr::beep(5)
  Sys.sleep(5)
  
   
  hs2_resume <- substr(selected_code, 1, 2) %>% as.integer() + 1
  # continue scrape after previous hs2 complete
  for (i in c(hs2_resume:97)) {
    tic()
    run_scrape(i, sleep_time = 4) %>% suppressMessages()
    toc()
    beepr::beep(3)
    Sys.sleep(5)
  }
}
map_hs_index <- function(code) {
  # code: 2, 4, 6 or 8 digit code used in the restart_scrape function
  code_str <- as.character(code)
  if(nchar(code_str) == 2){
    list_of_codes <- purrr::map(elem_hs2, function(x) x$getElementText()[[1]]) %>% unlist()
    list_index <- which(list_of_codes == substr(code_str, 1, 2))
    return(list_index)}
  else if(nchar(code_str) == 4){
    list_of_codes <- purrr::map(elem_hs4, function(x) x$getElementText()[[1]]) %>% unlist()
    list_index <- which(list_of_codes == substr(code_str, 1, 4))
    return(list_index)}
  else if(nchar(code_str) == 6){
    list_of_codes <- purrr::map(elem_hs6, function(x) x$getElementText()[[1]]) %>% unlist()
    list_index <- which(list_of_codes == substr(code_str, 1, 6))
    return(list_index)}
  else if(nchar(code_str) == 8){
    list_of_codes <- purrr::map(elem_hs8, function(x) x$getElementText()[[1]]) %>% unlist()
    list_index <- which(list_of_codes == substr(code_str, 1, 8))
    return(list_index)}
}
# Variables ---------------------------------------------------------------

# Column names for scraped table (before bind)
html_col_names <- c("name",
                    "País",
                    "Enero",
                    "Febrero",
                    "Marzo",
                    "Abril",
                    "Mayo",
                    "Junio",
                    "Julio",
                    "Agosto",
                    "Septiembre",
                    "Octubre",
                    "Noviembre",
                    "Diciembre")
