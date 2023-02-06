# get list of accreditted universties and cities
require(rvest)
require(tibble)

url = "https://www.collegesanddegrees.com/college-profiles?page="

# This website is fucked -- will need to scrape wikipedia

accrd_tibble <- list()
for(i in 1:78){
  accrd_tibble <- c(accrd_tibble,
                    paste0(url, i) |> 
                      read_html() |> 
                      html_elements('tbody') |> 
                      html_table())
}

out_tib <- accrd_tibble |> 
  lapply(function(x) as_tibble(x)) |> 
  
out_tib <- out_tib |> 
  do.call(what = rbind,)

names(out_tib) = c('Uni','City')

# |- Cleaning up city names -----------------
out_tib$City <- gsub('\n', '', out_tib$City)

# Only keep simple cities list
usable_cities <- grep('^([^,]+),\\s([A-Z]{2})$', out_tib$City)

trimmed_cities <- out_tib[usable_cities,]

write.table(trimmed_cities,'./data/00_accreditted-unis.tsv',
            row.names = F, sep = '\t')


##
# Get College List from USNEWS

library(rvest)
library(httr)
library(jsonlite)

fields <- c(
  'institution.displayName',
  'institution.schoolType',
  'institution.aliasNames',
  'institution.state',
  'institution.city',
  'institution.zip',
  'institution.region',
  'institution.isPublic',
  'institution.institutionalControl',
  'institution.primaryPhotoCard',
  'ranking.displayRank',
  'ranking.sortRank',
  'ranking.isTied',
  'searchData.actAvg.rawValue',
  'searchData.percentReceivingAid.rawValue',
  'searchData.acceptanceRate.rawValue',
  'searchData.tuition.rawValue',
  'searchData.hsGpaAvg.rawValue',
  'searchData.engineeringRepScore.rawValue',
  'searchData.parentRank.rawValue',
  'searchData.enrollment.rawValue',
  'searchData.businessRepScore.rawValue',
  'searchData.satAvg.rawValue',
  'searchData.costAfterAid.rawValue',
  'searchData.testAvgs.displayValue.0.value',
  'searchData.testAvgs.displayValue.1.value'
)

DETAILED <- TRUE
DETAIL_FIELDS <- c(
  'School Type',
  'Year Founded',
  'Religious Affiliation',
  'Academic Calendar',
  'Setting',
  '2018 Endowment',
  'School Website'
)

HEADERS <- c(
  'User-Agent'='Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:67.0) Gecko/20100101 Firefox/67.0'
)

traverse <- function(root, path) {
  value <- root
  segments <- strsplit(path, "\\.")[[1]]
  for (segment in segments) {
    if (segment %in% as.character(1:length(value))) {
      value <- value[[as.integer(segment)]]
    } else {
      value <- value[[segment]]
    }
  }
  value
}

fetch_results_page <- function(url, data_file) {
  print(paste0("Fetching ", url, "..."))
  resp <- httr::GET(url, httr::add_headers(HEADERS))
  json_data <- jsonlite::fromJSON(content(resp, "text"))
  for (school in json_data$data$items) {
    row <- sapply(FIELDS, function(field) traverse(school, field))
    
    if (DETAILED) {
      resp <- httr::GET(
        paste0("https://www.usnews.com/best-colleges/",
               traverse(school, "institution.urlName"), "-",
               traverse(school, "institution.primaryKey")),
        httr::add_headers(HEADERS)
      )
      soup <- rvest::html_session(content(resp, "text"))
      for (field in DETAIL_FIELDS) {
        
        
        library(httr)
        library(rvest)
        
        # Define the fields to scrape
        fields <- c("institution.displayName",
                    "institution.state",
                    "institution.city",
                    "institution.zip",
                    "institution.region",
                    "institution.isPublic",
                    "institution.institutionalControl")
        
        # Define the headers for the request
        headers <- c(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:67.0) Gecko/20100101 Firefox/67.0")
        
        # Function to traverse the JSON data structure
        traverse <- function(root, path) {
          value <- root
          segments <- strsplit(path, "\\.")[[1]]
          for (segment in segments) {
            if (segment %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")) {
              value <- value[[as.integer(segment) + 1]]
            } else {
              value <- value[[segment]]
            }
          }
          return(value)
        }
        
        # Function to fetch the results page
        fetch_results_page <- function(url, writer) {
          print(paste0("Fetching ", url, "..."))
          resp <- GET(url, headers = headers)
          json_data <- fromJSON(content(resp, as = "text"))
          for (school in json_data$data$items) {
            row <- c()
            for (field in fields) {
              row <- c(row, traverse(
