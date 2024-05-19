# 1 Loading packages ------------------------------------------------------
# remotes::install_github("jgcri/rgcam")
library(rgcam)
library(tidyverse)
library(data.table)
library(assertthat)
library(hector)

# * 1.1 Loading helper functions and reading in mapping data --------------

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

# Importing mapping file (will use for unit conversion)
emissions_map <- read.csv("data/GCAM_hector_emissions_map.csv")

# 2 Test Loading GCAM project data with loadProject ----------------------------

gcam_proj <- loadProject("data/gcam_emissions.dat")

gcam_proj

# 3 Using Kalyn's code as a guide to convert GCAM emissions to Hector emissions-----------------------------------------------------------------------

get_gcam_emissions <- function(dat_file) {
  # checking to make sure the data file is the right format
  
  # make sure rgcam file exists and load it
  assertthat::assert_that(file.exists(dat_file))
  gcam_res <- loadProject(dat_file)
  
  # pull out the emissions data from the dat_file
  gcam_df <- gcam_res$GCAM$`all emissions by region`
  
  # aggregate region emissions to get global totals
  global_totals <- gcam_df %>% 
    group_by(year, ghg) %>% 
    summarize(value = sum(value)) %>% 
    ungroup()
  
  # add emissions mapping information to the data frame
  gcam_emissions_map <- merge(global_totals, emissions_map, by = "ghg", all.x = T)
  setDT(gcam_emissions_map)

  ## from Kalyn's code -- check to make sure that the emissions being passed to Hector are
  ## the ones we are expecting to be passed to hector.
  ## We should only be expecting NAs for -- H2, H2_AWB, PM10, PM2,5
  no_matches <- unique(gcam_emissions_map[is.na(gcam_emissions_map$agg.gas), ]$ghg)
  expected_emissions <- c("H2", "H2_AWB", "PM10", "PM2.5", "CO2_FUG")
  assertthat::assert_that(all(no_matches %in% expected_emissions), msg = "unexpected emissions not being passed to Hector.")

  # We need to convert the GCAM emissions to Hector emissions (we use the merged mapping data for this) 
  gcam_emissions_map$converted_value <- gcam_emissions_map[ , list(value * unit.conv)]
  
  # Halocarbons can be aggregated into a single halocarbon category -- I think this line of code does that (from Kalyns code)
  gcam_inputs_for_hector <- gcam_emissions_map[ , list(value = sum(converted_value)), by = c( "agg.gas", "hector.name", "year", "hector.units")]
  
  # Drop the expected NAs 
  d <- na.omit(gcam_inputs_for_hector)
  
  # Error check here -- will add later

  # The expected years of data we want are from 2005-2100, before 2005 Hector is using GCAM inputs
  expected_years <- data.table(year = 2005:2100)

  # Construct a df of all the variable for 2005-2100. This will create a df with NAs when no GCAM emissions
  # are available - this data will be filled-in in the following step.
  save_cols <- names(d)[!names(d) %in% c("year", "value")]
  to_replicate <- distinct(d [, ..save_cols])
  df_with_all_yrs <- repeat_add_columns(x = to_replicate, y = expected_years)
  df_NA <- d[df_with_all_yrs, on = names(df_with_all_yrs), nomatch = NA]

  df_NA
  
  # Replace the NA emissions with linearly interpolated values
  df_list <- split(
    x = df_NA,
    f = interaction(df_NA$hector.name, df_NA$hector.units, drop = T))
  df_list
  
  complete_hector_emissions <- lapply(df_list, function(x) {
     new_vals <- zoo::na.approx(object = x$value, x = x$year)
     x$value <- new_vals
     return(x)
   }) %>%
     rbindlist
   
   # Format the data
   out <- complete_hector_emissions[, .(scenario = "GCAM", variable = hector.name, year, value, units = hector.units)]
   
   out
}

hector_emissions_data <- get_gcam_emissions("data/gcam_emissions.dat")
write.csv(hector_emissions_data, "data/gcam_emissions.csv", quote = FALSE)

# 4. Run Hector with the GCAM set up & emissions! -- SEE Kalyn script for help -----------------------------------------
use_gcam_emissions <- function(ini_path, emissions_df, 
                               out_vars = c(GMST(), RF_TOTAL(), CONCENTRATIONS_CO2())){
  
  # There should only be one scenario per emissions data frame. 
  assert_that(length(unique(emissions_df$scenario)) == 1)
  # TODO check to make sure that all the required emissions are included? 
  # TODO add a check that makes sure all the required columns are included in emissions_df? 
  
  # Set up the Hector core
  core <- newcore(ini_path, name = unique(emissions_df$scenario))
  setvar(core = core, 
         dates = emissions_df$year, 
         var = emissions_df$variable, 
         values = emissions_df$value, 
         unit = emissions_df$units)
  reset(core)
  
  # The below TODO is relevant fro me because I have LUC emissions, but need to format it and I want to see if this works first.
  # TODO this should be dropped when the dacccs & luc stuff is implemented in GCAM & in the query.
  luc_emissions <- data.frame(year = unique(emissions_df$year), 
                              var = LUC_EMISSIONS(), 
                              values = .5, 
                              unit = getunits(FFI_EMISSIONS()))
  setvar(core = core, 
         dates = 2005:2100, 
         var = luc_emissions$var, 
         values = 10, 
         unit = getunits(FFI_EMISSIONS()))
  reset(core)
  
  luc_uptake <- data.frame(year = 2005:2100,
                           var = LUC_UPTAKE(), 
                           values = 0, 
                           unit = getunits(FFI_EMISSIONS()))
  setvar(core = core, 
         dates = luc_uptake$year,
         var = LUC_UPTAKE(), 
         values = 0, 
         unit = getunits(FFI_EMISSIONS()))
  reset(core)
  
  # why the fuck is this still not running??? 
  reset(core)
  run(core, runtodate = 2050)
  out <- fetchvars(core = core, dates = 2005:2050, vars = out_vars)
  return(out)
  
}

hector_gcam_driven <- use_gcam_emissions(ini_path = "data/emissions_input.ini", 
                                         emissions_df = hector_emissions_data)
