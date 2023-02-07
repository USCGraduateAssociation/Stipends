# Read in the USC graduate programs

library(rvest)

usc_url <- 'https://sc.edu/study/colleges_schools/graduate_school/apply/degree_programs-application-requirements/index.php'

usc_grad_progs <- usc_url |> 
  read_html() |> 
  html_elements('tbody') |> 
  html_table()

usc_grad_progs <- usc_grad_progs[[1]][,c(1:3)]
names(usc_grad_progs) <- c('Program','Level','College')
