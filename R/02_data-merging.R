rm(list = ls())
library(readr)
library(fuzzyjoin)
library(stringr)
library(dplyr)
source('./R/utils.R')

uni_list <- read_tsv('./data/00_accreditted-unis.tsv',
         show_col_types = F)
           
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

uni_cost <- uni_list |> 
  left_join(metros_list)

# |- Matching Unis to unis --------------------

# format for better results
phd_stipends$uni <- gsub(' \\(.*\\)$', '',phd_stipends$uni)

test <- phd_stipends |> 
  stringdist_right_join(uni_cost, by = c('uni' = 'Uni'),method = 'jw',
                        max_dist = 0.1, distance_col = 'dist')

temp <- test |> 
  group_by(test$uni) |> 
  slice_min(dist)
