rm(list = ls())
library(readr)
library(fuzzyjoin)
library(stringr)
library(dplyr)
source('./R/utils.R')

uni_list <- read_csv('./data/00_usnews-unis.csv',show_col_types = F)

# |- Format unis ----------------------------
# only keep national universities in known states
uni_list <- uni_list |> 
  filter(institution.schoolType == 'national-universities') |> 
  filter(!is.na(institution.state))

# add city, state format
# this is due to how I previously wrote the code, not best way
uni_list$City <- paste(uni_list$institution.city, uni_list$institution.state,
                       sep = (', '))

           
metros_list <- read_tsv('./data/00_metro-area-wages.tsv',
                        show_col_types = F)

phd_stipends <- read_tsv('./data/00_phdsalaries-survey.tsv',
                         show_col_types = F)

names(metros_list) <- c('metro_area', 'living','poverty')

# Matching and data formatting ###############

# |- Matching cities to metros ---------------
 
#match unis to their metro_areas
uni_list$metro_area <- uni_list$City |> 
  sapply(city_to_metro, metros_list$metro_area) |> 
  unlist()

# trim down unis to just those that match
uni_list <- uni_list[!is.na(uni_list$metro_area),]

#format to keep ranking
uni_list$ranking.sortRank[uni_list$ranking.sortRank < 0] <- NA

uni_cost <- uni_list |>
  rename(Uni = institution.displayName,
         rank = ranking.sortRank) |> 
  select(Uni, City, metro_area, rank) |> 
  left_join(metros_list)

# |- Matching Unis to unis --------------------

# format for better results
phd_stipends$uni <- gsub(' \\(.*\\)$', '',phd_stipends$uni) # drop parentheses
phd_stipends$uni <- gsub('&amp;amp', '&', phd_stipends$uni) # swap &

stipends_by_city <- phd_stipends |> 
  stringdist_right_join(uni_cost, by = c('uni' = 'Uni'),method = 'jw',
                        max_dist = 0.075, distance_col = 'dist') |> 
  group_by(uni) |> 
  slice_min(dist)

# There's one case of arkansas matching to kansas state
stipends_by_city <- stipends_by_city[-grep('Arkansas State',stipends_by_city$uni),]

# |- Merging to our comparable programs --------------
usc_grad_progs <- read.table('./data/01_usc-programs.tsv', sep = '\t')

# brute force fixing
stipends_by_city$program[grep('Anatomy|Neuroscience|Biology|Botany',
                              stipends_by_city$program,
                              ignore.case = T)] <- 'Biological'

stipends_by_city$program[grep('Applied',
                              stipends_by_city$program,
                              ignore.case = T)] <- gsub('Applied',"",
                                stipends_by_city$program[grep('Applied',
                                                              stipends_by_city$program,
                                                              ignore.case = T)],
                                ignore.case = T)

stipends_by_city$program[grep('Atmospheric | Ocean | Earth',
                              stipends_by_city$program,
                              ignore.case = T)] <- 'SEOE'

stipends_by_city$program[grep('Psychology',
                              stipends_by_city$program,
                              ignore.case = T)] <- 'Psychology'

stipends_by_city$program[grep('BME',
                              stipends_by_city$program,
                              ignore.case = T)] <- 'Biomedical Engineering'

# Fuzzying matching

usc_grad_progs <- usc_grad_progs[!duplicated(usc_grad_progs$Program),]

stipends_by_program <- usc_grad_progs |>
  select('Program', 'College') |> 
  stringdist_right_join(stipends_by_city,
                       by = c('Program' = 'program'),
                       method = 'jw', max_dist = 0.1, 
                       distance_col = 'dist2') |> 
  group_by(Program) |> 
  slice_min(dist2)


# Save data

write.table(stipends_by_program, './data/02_webphdstipends-cleaned.tsv',
            row.names = F, sep = '\t')
