# loading in a dataframe used to make and .ini file
ini_data <- read.csv("data/ssp119_emiss-constraints_rf.csv")

# create a vector of uniique emissions names from the gcam_emissions data frame
emission_names <- unique(gcam_emissions_df$variable)

# Loop thourgh each emissions name and update values in ini_data with emissions
# from gcam_emissions
convert_ini <- function(ini_data, hector_emissions_data) {
  
  # create a vector of uniique emissions names from the gcam_emissions data frame
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
        
        # find the corresponding date in df1
        ini_date <- which(ini_data$Date == year)
        
        # if the year exists in df1, update the value
        if(length(ini_date) > 0) {
          ini_data[ini_date, emission_in_ini] <- values_h_emissions[i]
        }
      }
    }
  }
  
  # return the updated ini_data
  return(ini_update)
}

# run the function
ini_update <- convert_ini(ini_data, gcam_emissions_df)
write.csv(ini_update, "data/emissions_input.csv")

## New function to convert luc emissions data into hector inputs.
## Much of this function is based on Kalyn's code for converting GCAM
## emission to hector emissions.

get_luc_emissions <- function(dat_file) {
  
  # Check that the gcam data file exists
  assertthat::assert_that(file.exists(dat_file))
  
  # load gcam project from dat_file
  gcam_proj <- loadProject(dat_file)
  
  # extract the luc_emissions from the gcam_dat file
  luc_df <- gcam_proj$GCAM$`LUC emissions by region`
  luc_df$variable <- "luc_emissions"
  
  # convert gcam luc_emissions to hector luc_emissions
  luc_df$units <- "Pg C/yr"
  conv.factor <- 0.001 # From MT C/yr to Pg C/yr
  luc_df$converted_value <- luc_df$value * conv.factor

  # Aggregate regions to global luc_emissions for hector
  global_luc <- luc_df %>%
    group_by(scenario, variable, year, units) %>%
    summarize(value = sum(converted_value)) %>%
    ungroup()
  
  ## Now we need to interpolate data for the years that are not in the gcam output
  ## Follow Kalyn's code for this step.
  
  # Expected years for the Hector luc_emissions will be from 2005:2100
  expected_years <- 2005:2100
  
  # create rows for years not in gcam data
  annual_luc <- global_luc %>% 
    complete(year = 1975:2100, nesting(scenario, variable, units), fill = list(value = NA)) %>% 
    filter(year >2004)
    
  new_vals <- zoo::na.approx(annual_luc$value, annual_luc$year)
  annual_luc$value <- new_vals
  
  return(annual_luc)
}

test <- get_luc_emissions("data/gcam_emissions.dat")
