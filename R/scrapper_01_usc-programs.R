# Read in the USC graduate programs

library(rvest)

usc_url <- 'https://sc.edu/study/colleges_schools/graduate_school/apply/degree_programs-application-requirements/index.php'

usc_grad_progs <- usc_url |> 
  read_html() |> 
  html_elements('tbody') |> 
  html_table()

usc_grad_progs <- usc_grad_progs[[1]][,c(1:3)]
names(usc_grad_progs) <- c('Program','Level','College')

# Add SEOE
usc_grad_progs <- rbind(usc_grad_progs, c('SEOE','Master\'s', 'Arts and Sciences')) |> 
  rbind(c('SEOE','Doctorate', 'Arts and Sciences'))

# remove dashes
usc_grad_progs$Program <- sub("^([^-]*).*", "\\1",usc_grad_progs$Program)
usc_grad_progs$Program <- sub("\\s+$", "", usc_grad_progs$Program)

write.table(usc_grad_progs,
            './data/01_usc-programs.tsv', sep = '\t')
