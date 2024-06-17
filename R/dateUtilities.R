
# utility function to support metaprogramming with date
addDaysQuery <- function(cdm,
                         variable, # name of the variable to use
                         number,  # number of days or years, use a negative number to subtract da
                         type, # must be year or day
                         name_style){ # note, absolute values of numbers will be used

  if(!type %in% c("day", "year")){
    cli::cli_abort("type must be day or year")
  }

  number <- as.integer(number)

  if(type == "day"){
      q <- glue::glue("clock::add_days({variable} , {(number)})")
    }

  if(type == "year"){
    if(omopgenerics::cdmSourceType(cdm) == "spark"){
      # https://github.com/darwin-eu-dev/IncidencePrevalence/issues/395
      number_days_to_years <- as.integer(number*365)
      q <- glue::glue("clock::add_days({variable}, {(number_days_to_years)})")
    } else {
      q <- glue::glue("clock::add_years({variable}, {(number)})")
    }
    }

  q %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name_style))
}

minusDaysQuery <- function(cdm,
                         variable, # name of the variable to use
                         number,  # number of days or years, use a negative number to subtract da
                         type, # must be year or day
                         name_style = NULL, # note, absolute values of numbers will be used
                         names = NULL){  # alternative to name_style, set of names

  if(!type %in% c("day", "year")){
    cli::cli_abort("type must be day or year")
  }

  number <- as.integer(number)


  if(type == "day"){
      q <- glue::glue("clock::add_days({variable} , {(number)})")
    }
  if(type == "year"){
    if(omopgenerics::cdmSourceType(cdm) == "spark"){
      # https://github.com/darwin-eu-dev/IncidencePrevalence/issues/395
      number_days_to_years <- as.integer(number*365)
      q <- glue::glue("clock::add_days({variable}, {(number_days_to_years)})")
    } else {
      q <- glue::glue("clock::add_years({variable} , {(number)})")
    }
    }

  q <-q %>%
    rlang::parse_exprs()

  if(!is.null(name_style)){
    q<-q %>%
      rlang::set_names(glue::glue(name_style))
  }

  if(!is.null(names)){
    q<-q %>%
      rlang::set_names(names)
  }

  q

}



#to solve note that "All declared Imports should be used."
redundant_fun <- function() {
 clock::add_days(as.Date("2000-01-01"), 1)
}



