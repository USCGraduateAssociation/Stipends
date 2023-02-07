# Utilities for project

#' Scrape multiple pages
#' 
library(dplyr)

make_tibble <- function(vect) {
  tibble(uni = vect[1],
         program = vect[2],
         stipend = vect[3],
         value = vect[4],
         year = vect[5],
         year_in = vect[6],
         comment = vect[7],
         mo_12 = vect[8],
         mo_9 = vect[9],
         mo_3 = vect[10],
         fees = vect[11])
}


#' Scrape from phdstipends.com
get_phd_stipends <- function() {
  
  #needed packages
  require(rvest)
  require(rjson)
  require(tibble)
  
  
  #initialize list
  all_data <- list() 
  idx <- 1
  temp_data <- paste0('https://www.phdstipends.com/data/',idx) |> 
    readLines() |> 
    fromJSON()
  
  #loop for all possible pages
  while(length(temp_data$data) > 1) {
    all_data <- c(all_data, temp_data$data)
    idx <- idx+1
    temp_data <- paste0('https://www.phdstipends.com/data/',idx) |> 
      readLines() |> 
      fromJSON()
  }
  
  out_tibble <- all_data |> 
    lapply(make_tibble) |> 
    do.call(what = rbind,)
  
  
  return(out_tibble)
}

# Pull in raw stipend data
raw_phdstipend_data <- get_phd_stipends()

# |- Filter out the bad responses -----------------------------

# filter out the bad responses
clean_stipends <- raw_phdstipend_data |> 
  filter(uni != "" & program != "" & stipend != "")

# filter out weird stipend numbers
clean_stipends$stipend <- clean_stipends$stipend |>
  gsub(pattern='\\$|,|-', replacement='',) |> 
  as.numeric()

# filter to a reasonalbe range of possible values
clean_stipends <- clean_stipends |> 
  filter(stipend > 8960 & stipend < 45000)

write_tsv(clean_stipends, './data/00_phdsalaries-survey.tsv')

