# Here I scrape the MIT living wage site
# Specifically, I look for the metro areas then get the living wage for a single individual
# The MIT people made a great site but did not make it fully open source with a clean API
# So I have to do some brute force scrapping

require(rvest)

# Get MIT Living wage states

main_url <- "https://livingwage.mit.edu/"


state_list <- main_url |> 
  read_html() |> 
  html_elements('ul')

state_links <- state_list[[2]] |> 
  html_elements('li') |> 
  html_elements('a') |> 
  html_attr('href')


metros_list <- c()


get_metros_list <- function(url) {
  metros <- url |> 
    read_html() |> 
    html_elements('div.metros') |> 
    html_elements('a') |> 
    html_attr('href')
  
  return(metros)
}

for(link in state_links) {
  metros_list <- c(
    metros_list,
    get_metros_list(paste0(main_url,link))
  )
}


get_single_living_wage <- function(url, hours_in_year = 2080) {
  
  full_page <- url |> 
    read_html()
  
  title <- full_page |> 
    html_elements('div.container') |>
    html_elements('h1') |> 
    html_text() |> 
    gsub(pattern = 'Living Wage Calculation for ',replacement = '',)
  
  
  raw_living_wage_info <- full_page |> 
    html_element('div.table-responsive') |> 
    html_element('tbody') |> 
    html_elements('tr.odd') |> 
    html_elements('td') |> 
    html_text()
  
  clean_single_wage <- raw_living_wage_info[2] |> 
    gsub(pattern = '\\s',replacement = '',) |> 
    gsub(pattern = '\\$',replacement = '',) |> 
    as.numeric()
  
  poverty_wage_raw <- full_page |> 
    html_element('div.table-responsive') |> 
    html_element('tbody') |> 
    html_elements('tr.even') |> 
    html_elements('td') |> 
    html_text()
  
  clean_poverty_hrly <- poverty_wage_raw[2] |> 
    gsub(pattern = '\\s',replacement = '',) |> 
    gsub(pattern = '\\$',replacement = '',) |> 
    as.numeric()
  
  return(list(
    metro = title,
    living = clean_single_wage * hours_in_year,
    poverty = clean_poverty_hrly * hours_in_year
  ))
}


metro_wages <- data.frame(
  metro_area = NULL,
  living = NULL,
  poverty = NULL
)

for(link in metros_list) {
  metro_url <- paste0(main_url, link)
  temp_metro <-  get_single_living_wage(metro_url)
  
  metro_wages <- rbind(metro_wages,
                       data.frame(metro_area = temp_metro$metro, 
                         living = temp_metro$living, 
                         poverty = temp_metro$poverty))
}


write.table(metro_wages, './data/00_metro-area-wages.tsv',sep = '\t', row.names = F)
