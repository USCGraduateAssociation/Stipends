# This script uses already cleaned data from the initial survey
library(dplyr)
library(fuzzyjoin)
source('./R/utils.R')

# load initial data
stipends <- read.csv('~/../OneDrive/Documents/UofSC/GSA/Data/program_stipends_trimmed.csv')
usc_grad_progs <- read.table('./data/01_usc-programs.tsv', sep = '\t')

# |- Basic Cleaning ----------------------
stipends <- stipends |> 
  filter(stipend_amt != "" & !is.na(stipend_amt))

# convert stipends to numeric only
stipends$stipend_amt <- gsub('\\.00', "", stipends$stipend_amt)

stipends$stipend_amt <- gsub("[^[:digit:]]", "", stipends$stipend_amt) |> 
  as.numeric()

# remove strangly large stipends
stipends <- stipends[stipends$stipend_amt < 35000,]

stipends$addtl_amt <- gsub('\\.00', "", stipends$addtl_amt)

stipends$addtl_amt <- gsub("[^[:digit:]]", "", stipends$addtl_amt) |> 
  as.numeric()

stipends$addtl_amt[is.na(stipends$addtl_amt)] <- 0

stipends$total_funding <- stipends$addtl_amt + stipends$stipend_amt

stipends$student_type <- stipends$student_type |> 
  sapply(function(x) switch(x,
                            'Doctoral student' = 'Doctorate',
                            'Master\'s student' = 'Master\'s',
                            'Professional Student (MD, JD, PharmD)' = 'other')) |> 
  unlist()

# drop the non-masters/phd students
stipends <- stipends[stipends$student_type != 'other',]

# manual tweaking
stipends$department <- gsub('Biology', 'Biological', stipends$department,
                            ignore.case = T)
stipends$department <- gsub('Mech Engr', 'Mechianical', stipends$department,
                            ignore.case = T)
stipends$department <- gsub('HPEB', 'Health Promotion', stipends$department,
                            ignore.case = T)
stipends$department <- gsub('Clinical', 'Psychology', stipends$department,
                            ignore.case = T)



# |- Program Merging ----------------------

#split out masters and phd
stipends_by_level <- stipends |> split(f = stipends$student_type)

stipends_by_program <- list()
for (level in names(stipends_by_level)) {
  
  stipends_by_program[[level]] <- stipends_by_level[[level]] |> 
    stringdist_left_join(usc_grad_progs[usc_grad_progs$Level == level,],
                         by = c('department' = 'Program'),
                         method = 'jw', max_dist = 0.25, 
                         distance_col = 'dist') |> 
    group_by(department) |> 
    slice_min(dist)
  
}

stipends_by_program <- do.call(rbind, stipends_by_program)

# View(cbind(stipends_by_program$department, stipends_by_program$Program)) #check

#frustrating tab
stipends_by_program$Program[grep('Health Promotion',stipends_by_program$Program)] <- 
  'Health Promotion, Education, and Behavior'

stipends_by_program$program_type <- program_type_switch(stipends_by_program$Program)

# save clean stipend data
write.table(stipends_by_program, './data/01_clean-stipends.tsv',
            sep = '\t')