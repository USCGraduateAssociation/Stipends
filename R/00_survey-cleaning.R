# Importing and cleaning USC data

rm(list = ls())
require(dplyr)
require(tidyr)

# for privacy, I'm not sharing the raw data
raw_surv <- read.csv('~/../OneDrive/Documents/UofSC/GSA/Data/GSA Cost of Living and Stipend raw data.csv')


# |- Format cost of living data ------------------

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

# Trim non-response rows
cost_of_living <- cost_of_living |> 
  filter(rent_mo != "" & util_mo != "" & food_mo != "" & transp_mo != "" &
           healthCare_mo != "")

clean_with_others <- function(df, cols, names_to, values_to,
                              other_col) {
  # clean the other column
  no_race_idx <- df[,cols] |> 
    apply(1, function(x) all(x == '')) |> 
    which()
  
  answered_other_idx <- df[, other_col] |> 
    sapply(function(x) x != '') |> 
    which()
  
  answered_both_idx <- which(!(answered_other_idx %in% no_race_idx))
  
  
  # set other race to just primary
  df[answered_both_idx, other_col] <- ''
  
  
  df <- df |> pivot_longer(cols = c(cols, other_col),
                           names_to = names_to,
                           values_to = values_to)
  
  df <- df[, -which(names(df) == names_to)]
  df <- df[-which(df[[values_to]] == ''),]
  
  # remove any duplicated repsonses
  if(any(duplicated(df$RespondentId))) {
    df <- df[-which(duplicated(df$RespondentId)),]
  }
  return(df)
}

cost_of_living <- cost_of_living |> 
  clean_with_others(cols= c('is_white','is_black', 'is_asian','is_native',
                            'is_pacIsland','is_hispanic','is_mixed'),
                    other_col = 'otherRace',
                    names_to = 'RaceQ',
                    values_to = 'Race')
cost_of_living <- cost_of_living |> 
  clean_with_others(cols = c("is_ciswoman",'is_cisman','is_transwoman',
                             'is_transman', 'is_nonbinary'),
                    other_col = 'otherGen',
                    names_to = 'GenQ',
                    values_to = 'Sex_Gen')

# Hard formatting race
cost_of_living$Race[grep('Middle',cost_of_living$Race)] <- 'Middle Eastern'
cost_of_living$Race[grep('Lebanese', cost_of_living$Race)] <- 'Middle Eastern'
cost_of_living$Race[grep('Indian', cost_of_living$Race)] <- 'Asian'
cost_of_living$Race[grep('Human|answer', cost_of_living$Race)] <- NA
cost_of_living$Race[grep('Afro|Native|Mixed', cost_of_living$Race)] <- 'Other BIPOC'
cost_of_living$Race[grep('Caucasian', cost_of_living$Race)] <- 'White'

# save the data
write.table(cost_of_living, './data/01_cost-of-living.tsv',
            sep = '\t')

# |- Format Stipends ---------------------
program_stipends <- raw_surv[,c(1, 8, 10:15, 18,19,21)]

names(program_stipends) <- c(
  'RespondentId',
  'student_type',
  'department',
  'tuition_covered',
  'stipend_type',
  'contract_position',
  'funding_source',
  'contract_term',
  'stipend_amt',
  'addtl_funding',
  'addtl_amt'
)

# trim non-useful responses
program_stipends <- program_stipends |> 
  filter(department != "") |> #remove non-reports
  filter(stipend_type == 'Yes') |>  #remove non-stipend recipients
  filter(funding_source != 'External (outside USC)') #drop external funds

# write temporary
write.csv(program_stipends,'~/../OneDrive/Documents/UofSC/GSA/Data/program_stipends_trimmed.csv',
          row.names = F)
