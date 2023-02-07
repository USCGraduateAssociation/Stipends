# Importing and cleaning USC data

rm(list = ls())
require(dplyr)
require(tidyr)

# for privacy, I'm not sharing the raw data
raw_surv <- read.csv('~/../OneDrive/Documents/UofSC/GSA/Data/GSA Cost of Living and Stipend raw data.csv')



cost_of_living <- raw_surv[,c(1,23,25:54)]
names(cost_of_living) <- c(
  'RespondentId', #respondant ID
  'living_situation', #living situation
  'rent_mo',
  'util_mo',
  'food_mo',
  'transp_mo',
  'healthCare_mo',
  'household_items_mo',
  'take_loans',
  'recieve_fin_support',
  'provide_fin_support',
  'expenditures_exceed_income',
  'mental_health_stipend_impact',
  'food_insecurity',
  'residency_status',
  'is_white',
  'is_black',
  'is_asian',
  'is_native',
  'is_pacIsland',
  'is_hispanic',
  'is_mixed',
  'is_otherRace',
  'otherRace',
  'is_ciswoman',
  'is_cisman',
  'is_transwoman',
  'is_transman',
  'is_nonbinary',
  'is_other',
  'otherGen',
  'parental_education'
)


