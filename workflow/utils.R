
# 1 Loading packages ------------------------------------------------------

# remotes::install_github("jgcri/rgcam", build_vignettes = T)
library(rgcam)
library(tidyverse)
library(data.table)
library(assertthat)
library(hector)
library(matilda)

# 2 Helper functions -------------------------------------------------------

# data.table implementation of the gcamdata repeat_add_columns
# Args 
#   x: data.table to add to 
#   y: data.table containing the column that should be repeated and added to dt x
# return: data.table
repeat_add_columns <- function(x, y){
  
  assert_that(is.data.table(x))
  assert_that(is.data.table(y))
  assert_that(!any(names(x) %in% names(y)))
  assert_that(!any(names(y) %in% names(x)))
  
  x$join <- 1
  y$join <- 1
  
  df <- merge(x, y, all = TRUE, by = .EACHI, allow.cartesian=TRUE)
  df$join <- NULL
  return(df)
  
}

# 3 Function to get GCAM emissions for Hector ----------------------------

get_hector_emissions <- function(gcam_emissions_file){
  
  # make sure rgcam file exists and load it
  assertthat::assert_that(file.exists(gcam_emissions_file))
  gcam_data <- loadProject(gcam_emissions_file)
  
  # pull out the emissions data from the dat_file
  gcam_df <- gcam_data$GCAM$`all emissions by region`
  
  # aggregate regional emissions to get global emissions totals
  global_emissions <- aggregate(value ~ year + ghg,
                                data = gcam_df,
                                FUN = sum)
  
  # get emissions mapping information
  emissions_map <- read.csv("workflow/data/GCAM_hector_emissions_map.csv") 
  
  # merge emissions_map with global_emissions
  gcam_emissions_map <- merge(global_emissions, emissions_map, by = "ghg", all.x = TRUE) # all.x = T ensures NAs are added where ghg in global_emissions is not in emissions_map
  setDT(gcam_emissions_map)
  
  # check that expected emissions are being passed to Hector.
  # Expect NAs for:  H2, H2_AWB, PM10, PM2.5, and I think CO2_FUG
  # TODO: what to do in CO2_FUG situation
  missing_ghgs <- unique(gcam_emissions_map[is.na(gcam_emissions_map$agg.gas), ]$ghg)
  expected_missing_ghgs <- c("H2", "H2_AWB", "PM10", "PM2.5", "CO2_FUG")
  
  # check for the presence of expected_missing_ghgs in missing_ghgs.
  # if any of the expected missing ghgs are not found in missing_ghgs, send an error. 
  if(!all(expected_missing_ghgs %in% missing_ghgs)) {
    stop ("Some emissions being passed to Hector are unexpected. These emissions may not be coded in Hector.")
  }
  
  # convert gcam emissions to to hector emissions:
  # 1 unit conversion
  gcam_emissions_map$converted_value <- gcam_emissions_map$value * gcam_emissions_map$unit.conv
  
  # 2 Halocarbon ghgs can be aggregated into a single halocarbon category
  sum_halocarbon <- gcam_emissions_map[, list(value = sum(converted_value)), 
                                       by = c("agg.gas", "hector.name", "year", "hector.units")]
  
  # omit NAs from the halocarbon aggregate
  gcam_emissions_input <- na.omit(sum_halocarbon)
  
  # Check for important columns
  required_columns <- c("year", "value")
  if (!all(required_columns %in% names(gcam_emissions_input))){
    stop("hmmm, there is an important column missing is there a year and value column present?")
  }
  
  # establish data years; 2005:2100. Before 2005 Hector uses GCAM inputs
  data_years <- data.table(year = 2005:2100)
  
  # TODO: Is there a better way to do this (lines 94-97)
  # Construct data frame of all the variables for the 2005:2100 year range.
  # This code creates a data frame with NAs when no GCAM emissions are available.
  # The NAs will be subsequently infilled.
  columns_to_save <- names(gcam_emissions_input)[!names(gcam_emissions_input) %in% required_columns]
  data_to_replicate <- distinct(gcam_emissions_input[, ..columns_to_save])
  data_with_target_years <- repeat_add_columns(x = data_to_replicate, y = data_years)
  NA_emissions <- gcam_emissions_input[data_with_target_years, on = names(data_with_target_years), nomatch = NA]
  
  # Replace emission NAs with approximated values 
  approximated_emissions <- NA_emissions %>% 
    group_by(hector.name, hector.units) %>% 
    mutate(value = ifelse(is.na(value), 
                          approx(year, value, xout = year, rule = 2)$y, 
                          value)) %>% 
    ungroup() %>% 
    setDT()
  
  # construct final output
  hector_emissions <- approximated_emissions[, .(scenario = "GCAM",
                                                 variable = hector.name,
                                                 year = year,
                                                 value = value,
                                                 units = hector.units)]
  return(hector_emissions)
}

# 4 Function converting GCAM LUC emissions to Hector LUC emissions --------
## New function to convert luc emissions data into hector inputs.
## Function is based on GCAM Land C code and Kalyns code.

get_luc_emissions <- function(gcam_emissions_file) {
  
  # Check that the gcam data file exists
  assertthat::assert_that(file.exists(gcam_emissions_file))
  
  # load gcam project from dat_file
  gcam_proj <- loadProject(gcam_emissions_file)
  
  # extract the luc_emissions from gcam_emissions_file
  luc_df <- gcam_proj$GCAM$`LUC emissions by region`
  luc_df$variable <- "luc_emissions"
  
  # convert gcam luc_emissions to hector luc_emissions
  luc_df$units <- "Pg C/yr"
  conv.factor <- 0.001 # From MT C/yr to Pg C/yr
  luc_df$converted_value <- luc_df$value * conv.factor
  
  # Aggregate regions to global luc_emissions for Hector
  global_luc <- luc_df %>%
    group_by(scenario, variable, year, units) %>%
    summarize(value = sum(converted_value)) %>%
    ungroup()
  
  # Expected years for the Hector luc_emissions will be from 2005:2100
  # wait...am I not using this anywhere?
  expected_years <- 2005:2100
  
  # create rows for years not in gcam data
  annual_luc <- global_luc %>% 
    complete(year = 1975:2100, # use complete() to get complete years in the df and fill with NAs
             nesting(scenario, variable, units), 
             fill = list(value = NA)) %>% 
    filter(year > 2004) %>% 
    # only interested in 2005 -- before 2005 Hector uses gcam emissions?
    # TODO: get confirmation about filtering in line 152
    mutate(value = ifelse(is.na(value), 
                          approx(year, value, xout = year, rule = 2)$y,
                          value)) %>% 
    ungroup() %>% 
    select(scenario, variable, year, value, units)
  
  return(annual_luc)
}


# 5 Function building input document (csv) "get_emissions_constraints" -----------------------

# Loop through each emissions name and update values in ini_data with emissions
# from gcam_emissions
get_emissions_constraints <- function(hector_emissions_data) {
  
  # load in ini_data -- this will be moot once this set of functions is a package
  # TODO: This should be package data
  ini_data <- read.csv("workflow/data/ssp119_emiss-constraints_rf.csv", stringsAsFactors = F, skip = 5)
  
  # create a vector of unique emissions names from the hector emissions data 
  emission_names <- unique(hector_emissions_data$variable)
  
  # initialize the ini_update df (copy of the ini_data)
  ini_update <- ini_data
  
  # Loop through each emissions name and update values in ini_data with emissions
  # from gcam_emissions
  
  for (emission in emission_names) {
    # find the emission in ini_data
    emission_in_ini <- grep(emission, colnames(ini_data))
    
    # if the emission is found in ini_data, match the date to the corresponding year in the gcam emissions df
    if( length(emission_in_ini > 0)) {
      # extract the values from the gcam_emissions df for the current emission
      values_h_emissions <- hector_emissions_data$value[hector_emissions_data$variable == emission]
      years_h_emissions <- hector_emissions_data$year[hector_emissions_data$variable == emission]
      
      # Loop through each year in gcam_emissions
      for (i in seq_along(years_h_emissions)) {
        year <- years_h_emissions[i]
        
        # find the corresponding date in ini_data
        ini_date <- which(ini_data$Date == year)
        
        # if the year exists in ini_data, update the value
        if(length(ini_date) > 0) {
          ini_update[ini_date, emission_in_ini] <- values_h_emissions[i]
        }
      }
    }
  }
  
  # return the updated ini_data
  return(ini_update)
}

# 6 Function editing input document (ini) -------------------------------------


## TODO if this will be a new package I would probably add an original emissions constraint file to the 
write_emissions_constraint_file <- function(hector_emissions_path,
                                            old_emissions_constraint,
                                            new_name = "gcam_emissions",
                                            directory) {
  
  # Check that the hector emissions file exist
  assertthat::assert_that(file.exists(hector_emissions_path), msg = "Hector emissions file does not exist.")
  
  # read the first five lines of the original emissions constrain data
  header <- readLines("workflow/data/ssp119_emiss-constraints_rf.csv", n = 5)
  
  # readLines of the new emissions constraints (only arg in this function, should be csv)
  new_emission_data <- readLines(hector_emissions_path)
  
  # add the header to the emissions_constraint_data
  new_emission_data <- c(header, new_emission_data)
  
  # change first line to new emissions information 
  new_emission_data <- gsub("ssp119 from rcmip", "emissions from gcam run", new_emission_data)
  
  # check if output directory exists
  assert_that(dir.exists(directory), msg = "Output directory does not exist.")
   
  # write the new lines and save
  output_file <- file.path(directory, paste0(new_name, ".csv"))
  writeLines(new_emission_data, output_file)
}

# 8 Function to write input file ----------------------------------------

## the goal here is for the args to be the path of the new emissions constraint file -- anything else?
## The idea is to take the file for the new emissions constraint and use it to change the original emissions input file.

write_input_file <- function(emissions_constraint_file, 
                             old_input_file = system.file("input/hector_ssp119.ini", package = "hector"),
                             new_name = "gcam_emissions", 
                             directory) {
  
  # check if file exists
  assertthat::assert_that(file.exists(emissions_constraint_file), 
                          msg = "The emissions constraints file specified does not exist.")
  
  # readLines of the old emissions file
  old_emissions_file <- readLines(old_input_file)
  
  # substitute old emissions constraint path to new emissions constraint path
  new_emissions_file <- gsub("csv:tables/ssp119_emiss-constraints_rf.csv", 
                             paste0("csv:", emissions_constraint_file), 
                             old_emissions_file)
  new_emissions_file <- gsub("ssp119",
                             new_name, 
                             new_emissions_file)
  
  # check if the output directory exists
  ## TODO should the function create a directory here if the user doesn't supply one?
  assertthat::assert_that(dir.exists(directory), msg = "Output directory does not exist.")
  
  # write lines for the new emissions ini
  output_file <- file.path(directory, paste0(new_name, ".ini"))
  writeLines(new_emissions_file, output_file)
}


# 9 The whole game -------------------------------------------------------

write_emissions_input <- function(gcam_data,
                                  old_emissions_constraint,
                                  new_name = "gcam_emissions",
                                  directory) {
  
  # get emissions for hector format
  hector_emissions <- get_hector_emissions("workflow/data/gcam_emissions.dat")
  
  # get luc emissions for hector format
  hector_luc_emissions <- get_luc_emissions("workflow/data/gcam_emissions.dat")
  
  # full emissions for hector
  full_hector_emissions <- rbind(hector_emissions, hector_luc_emissions)
  
  # format emissions constraints
  emissions_constraints <- get_emissions_constraints(full_hector_emissions)
  
  # write .csv of emissions constraints
  hector_emissions_file <- file.path(directory, paste0(new_name, "_noheader.csv"))
  write.csv(emissions_constraints, hector_emissions_file, row.names = F, quote = F)
  
  # write new gcam emissions constraint .csv with header
  write_emissions_constraint_file(hector_emissions_path = hector_emissions_file,
                                  old_emissions_constraint = old_emissions_constraint,
                                  new_name = new_name,
                                  directory = directory)
  # access new gcam_emissions_constraint file
  gcam_emissions_file <- file.path(directory, paste0(new_name, ".csv"))
  
  # write new gcam emissions constrain .ini file
  write_input_file(emissions_constraint_file = gcam_emissions_file,
                   new_name = new_name,
                   directory = directory)
  
}

