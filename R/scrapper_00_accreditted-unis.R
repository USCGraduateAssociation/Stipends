# get list of accreditted universties and cities
require(rvest)
require(tibble)

url = "https://www.collegesanddegrees.com/college-profiles?page="

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
