# function piece by piece

# Pull out the emissions data from the project file 
gcam_df <- gcam_proj$GCAM$`all emissions by region`

# aggregate region emssions to get global totals
## with tidyr
global_totals <- gcam_df %>% 
  group_by(year, ghg) %>% 
  summarize(value = sum(value)) %>% 
  ungroup()
## in base
global_totals2 <- aggregate(value ~ year + ghg, 
                            data = gcam_df, 
                            FUN = sum)

# Need to add emissions mapping infromation to the global totals data frame
# merge the emissions map df to the global totals df
gcam_emissions_map <- merge(global_totals, emissions_map, by = "ghg", all.x = T)
# by using all.x = T we are adding NA rows where the ghg in global totals is not in emissions map.
# Make this data a data.table
setDT(gcam_emissions_map)

# Make sure the emissions being passed to Hector are the ones we are expecting
# We should expect there to be NAs for -- H2, H2_AWB, PM10, PM2.5, and I think CO2_FUG (we can figure this one out later)
no_matches <- unique(gcam_emissions_map[is.na(gcam_emissions_map$agg.gas), ]$ghg)
# this gives us a character vector of ghgs that are NAs in the agg.gas column 
expected_missing_emissions <- c("H2", "H2_AWB", "PM10", "PM2.5", "CO2_FUG")
if (!all(expected_missing_emissions %in% no_matches)) {
  stop ("Some emissions being passed to Hector are unexpected.")
}
# condition checks for the presence of the "expected missing emissions" in "no_matches". If any of the expected missing emissions
# are not found in no_matches then send a message.

# Converting GCAM emissions to Hector emissions (use the mapping data)
gcam_emissions_map$converted_value <- gcam_emissions_map$value * gcam_emissions_map$unit.conv 
# this creates a new column of converted ghg emissions using the value and unit.conv columns

# Halocarbon ghgs can be aggregated into a single halocarbon category 
gcam_inputs_for_hector <- gcam_emissions_map[, list(value = sum(converted_value)),
                                             by = c("agg.gas", "hector.name", "year", "hector.units")]
# omit NAs
filtered_gcam_inputs <- na.omit(gcam_inputs_for_hector) 

# Check to make sure the required columns are present
required_cols <- c("year", "value")
if (!all(required_cols %in% names(filtered_gcam_inputs))){
  stop("hmmm, there is an important column missing is there a year and value column present?")
}

# The years of data that we want are from 2005-2100, before 2005 Hector is using GCAM inputs
data_years = data.table(year = 2005:2100)

# TODO: Is there a better way to do lines 56-63
# 
# Construct a data frame of all the variables for the 2005:2100 year range. This will create a dataframe with NAs
# when no GCAM emissions are available -- this data will be filled in next
save_cols <- names(filtered_gcam_inputs)[!names(filtered_gcam_inputs) %in% required_cols]
# this saves the column names that are not already in the required_cols object
data_to_replicate <- distinct(filtered_gcam_inputs[, ..save_cols])
# extract distinct data only from the columns specified in save_cols
df_with_all_years <- repeat_add_columns(x = data_to_replicate, y = data_years)
NA_emissions <- filtered_gcam_inputs[df_with_all_years, on = names(df_with_all_years), nomatch = NA]

# Replace NA emissions with approximated values 
emissions_approximated <- NA_emissions %>% 
  group_by(hector.name, hector.units) %>% 
  mutate(value = ifelse(is.na(value),
                        approx(year, value, xout = year, rule = 2)$y, 
                        value)) %>% 
  ungroup() %>% 
  setDT()

# construct ouput
complete_hector_emissions <- emissions_approximated[, .(scenario = "GCAM",
                                                        variable = hector.name,
                                                        year = year, 
                                                        value = value, 
                                                        units = hector.units)]

write.csv(complete_hector_emissions, "data/gcam_emissions_update.csv", row.names = FALSE, quote = FALSE)


# TODO -- What is Next?
# 1. Function to deal with header.
## a. get the header info of the original emissions data frame
### 1. use readLines()
## b. Add the header to the new data frame and save 
### 1. use readLines() of the new hector emission .csv
### 2. concatenate with header
### 3. use writeLines() to write the new lines 
## c. Replace old file path with new file path 
### 1. readLines() on the old file path
### 2. Subsititute old emissions path with new emissions path in the old emissions ini
### 3. write line for the new emissions ini