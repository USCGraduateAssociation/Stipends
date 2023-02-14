
## MATCHING FOR CITIES ##########################

#' Match City, ST
#' 
#' return a match based on a city, state combo
#' 
#' @param x character vector to match to 
#' @param ref character vector of references
match_cities <- function(x, ref) {
  require(dplyr)
  require(stringdist)
  
  # split out the state
  x_states <- x |> 
    strsplit(', ') |> 
    sapply(`[[`, 2)
  
  ref_states <- ref |> 
    strsplit(', ') |> 
    sapply(`[[`,2)
  
  #split cities out based on states
  x_by_state <- split(x, f = x_states)
  ref_by_state <- split(ref, f = ref_states)
  
  #loopthrough to get matchs
  metro_match <- list()
  for(state in unique(x_states)) {
    
    # to skip non-us cities
    if(!(state %in% names(ref_by_state))) {
      next()
    }
    
    unique_city <- x_by_state[[state]] |> 
      unique()
    
    metro_match[[state]] <- list()
    
    metro_match[[state]]$cities <- unique_city
    
    metro_match[[state]]$metro_areas <- unique_city |> 
      sapply(inner_city_match, ref_by_state[[state]])
  }
  
  # It's late and I'm wriitng sloppy code
  # issue with the current approach and names on columsn
  # So messy interior function will be used
  
  
}


#' Supporting matching function
#' 
#' find a perfect match for a city to a metro area
#' 
inner_city_match <- function(city, ref_cities) {
  matcher <- grep(city, ref_cities)
  if(length(matcher) == 0) {
    return(NA)
  } else {
    return(ref_cities[matcher])
  }
}


#' Match a city to a metro in the big list
#' 
#' @param city
inner_match_2 <- function(city) {
  
  state <- city |>  
    strsplit(', ') |> 
    sapply(`[[`, 2)
  
  city
    
}



#' City match to metro_area
#' 
#' @param city the city to match
#' @param metro_list all possible metro_areas
city_to_metro <- function(city, metro_list) {
  
  city_state <- city |> 
    strsplit(', ') |> 
    sapply(`[[`, 2)
  
  metro_states <- metro_list |> 
    strsplit(', ') |> 
    sapply(`[[`, 2)
  
  if(any(metro_states == city_state)) {
    possible_metros <- metro_list[which(metro_states == city_state)]
    
    city_only <- city |> 
      strsplit(', ') |> 
      sapply(`[[`,1)
    
    metro_match <- grep(city_only, possible_metros, value = T)
    
    if(length(metro_match) == 1) {
      return(metro_match)
    } else if(length(metro_match == 2)) {
      print(metro_match)
      return(metro_match[1])
    } else {
      return(NA)
    }
    
  } else {
    return(NA)
  }
}

#' Add programs to types
program_type_switch <- function(vect) {
  vect |> 
  sapply(function(x) switch(x,
                            'Anthropology' = 'Social Science',
                            'Biological Sciences' = 'Natural Science',
                            'Biomedical Engineering' = 'Engineering',
                            'Biomedical Science' = 'Natural Science',
                            'Biostatistics' = 'Natural Science',
                            'Business Administration' = 'Professional',
                            'Chemical Engineering' = 'Engineering',
                            'Chemistry' = 'Natural Science',
                            'Comparative Literature' = 'Humanities',
                            'Computer Engineering' = 'Engineering',
                            'Computer Science' = 'Engineering',
                            'Counselor Education' = 'Humanities',
                            'Epidemiology' = 'Natural Science',
                            'Criminology and Criminal Justice' = 'Social Science',
                            'Music Performance' = 'Humanities',
                            'Economics' = 'Social Science',
                            'Electrical Engineering' = 'Engineering',
                            'English' = 'Humanities',
                            'Environmental Health Sciences' = 'Natural Science',
                            'Exercise Science' = 'Natural Science',
                            'Geography' = 'Natural Science',
                            'Health Promotion, Education, and Behavior' = 'Social Science',
                            'Health Services Policy and Management' = 'Humanities',
                            'History' = 'Humanities',
                            'Hospitality Management' = 'Humanities',
                            'Journalism and Mass Communications' = 'Humanities',
                            'Library and Information Science' = 'Social Science',
                            'Linguistics' = "Humanities",
                            'Marine Science' = "Natural Science",
                            'Mathematics' = "Natural Science",
                            'Mechanical Engineering' = 'Engineering',
                            'Music Education' = "Humanities",
                            'Nursing Science' = 'Professional',
                            'Geological Sciences' = 'Natural Science',
                            'Philosophy' = 'Humanities',
                            'Physical Education' = "Humanities",
                            'Physics' = 'Natural Science',
                            'Political Science' = 'Social Science',
                            'Psychology' = 'Social Science',
                            'SEOE' = 'Natural Science',
                            'Social Work' = 'Professional',
                            'Special Education' = 'Professional',
                            'Sport and Entertainment Management' = 'Humanities',
                            'Statistics' = "Natural Science",
                            'Accountancy' = 'Professional',
                            'Athletic Training, Advanced' = "Professional",
                            'Aerospace Engineering' = 'Engineering',
                            'Art History' = "Humanities",
                            'Athletic Training' = "Natural Science",
                            'Business Analytics' = 'Professional',
                            'Creative Writing' = 'Humanities',
                            'Languages, Literatures, and Cultures' = 'Humanities',
                            'Educational Psychology and Research' = 'Humanities',
                            'Genetic Counseling' = 'Professional',
                            'Higher Education and Student Affairs' = 'Humanities',
                            'International Business' = 'Professional',
                            'Learning Design and Technologies' = 'Social Science',
                            'Media Arts' = 'Humanities',
                            'Music' = "Humanities",
                            'Public History' = 'Humanities',
                            'Sociology' = 'Humanities',
                            'Art Education' = 'Humanities',
                            'Theatre' = 'Humanities')) |> 
  unlist()
}
