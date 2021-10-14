# Set of functions for different parts of the scraping pipeline

# sleep_time <- 8
run_scrape <- function(hs2_code, hs4_code_start = NULL, sleep_time) {
  # Inputs: selected hs2_code to scrape, sleep time in seconds between each click of year or code
  # Inputs: (optional) hs4_code to start from
  # Outputs: returns dataframe with scraped data
  # Issues: this only works for one HS2 code at a time
  
  # Navigating to the URL = this is to refresh the page to avoid issue with hs2 code
  remDr$navigate(url)
  
  # Refresh live html along with remDr elements of interest (hs code list, years)
  refresh_elements()
  
  
  # initiate master df
  df_master <- initiate_df()
  
  # Testing elem codes - optional
  # sapply(elem_hs6, function(x) x$getElementText())

  # Select hs2 code
  # move mouse to hs2 code
  remDr$mouseMoveToLocation(webElement = elem_hs2[[hs2_code]])
  # pause until hs2 code is in view
  Sys.sleep(1)
  #click
  remDr$click(1)
  
  Sys.sleep(6)
  refresh_elements()
  
  # 1st loop - loop through HS4 codes
  #df_hs4 <<- initiate_df()
  for (i in hs4_code_start:length(elem_hs4)){
    
    # move mouse to hs4 code
    remDr$mouseMoveToLocation(webElement = elem_hs4[[i]])
    #click
    remDr$click(1)
    
    Sys.sleep(6)
    refresh_elements()
    
    df_hs6 <<- initiate_df()
    # 2nd loop - loop through HS6 codes
    for (i in 1:length(elem_hs6)) {
      
      # move mouse to hs6 code
      remDr$mouseMoveToLocation(webElement = elem_hs6[[i]])
      #click
      remDr$click(1)
      
      Sys.sleep(6)
      refresh_elements()
      
      # 3rd loop - loop through HS8 codes
      df_hs8 <<- initiate_df()
      for (i in 1:length(elem_hs8)) {
        # move mouse to hs8 code
        remDr$mouseMoveToLocation(webElement = elem_hs8[[i]])
        #click
        remDr$click(1)
        
        #print(paste0("Sleeping.... zzz ", Sys.time()))
        # sleep to wait for tables to load
        Sys.sleep(6)
        #print(paste0("Awake! ", Sys.time()))
        
        refresh_elements()
        
        # Store selected HS8 code
        selected_code <- elem_hs8[[i]]$getElementText()[[1]]
        
        # 5th loop - loop through each year (we only want 2020 and 2019)
        df_year <<- initiate_df()
        for (i in 2:3) {
          # Try/Catch for error handling when gathering table data 
          # This is required as sometimes the tables can take very long to load (30 seconds)
          # Only required for first table, as, if it loads, the others should also load
          
          # move mouse to year
          remDr$mouseMoveToLocation(webElement = elem_year[[i]])
          # and then click
          remDr$click(1)
          Sys.sleep(sleep_time)
          
          exports_valor <- NULL
          while(is.null(exports_valor)) {
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
    } # end of 2nd loop
    df_hs4 <<- bind_rows(df_hs4, df_hs6)
  } # end of 1st loop
  df_hs4 %>% write_csv(paste0("data/hs", hs2_code, "_data.csv"))
}

#df_hs4 %>% write_csv(paste0("data/hs", "39", "_data_incomplete.csv"))

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
  
  remDr$close()
  rD$server$stop()
  rm(rD)
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
