
# 1 source functions ------------------------------------------------------

source("workflow/utils.R")

# 2 Using the functions piecewise and editing ini -----------------------------------------------------

hector_emissions <- get_hector_emissions("workflow/data/gcam_emissions.dat")
hector_luc_emissions <- get_luc_emissions("workflow/data/gcam_emissions.dat")
full_hector_emissions <- rbind(hector_emissions, hector_luc_emissions)

emissions_constraints <- get_emissions_constraints(full_hector_emissions)

write.csv(emissions_constraints, "workflow/data/new_hector_emissions.csv", quote = FALSE, row.names = FALSE)

write_emissions_constraint_file(hector_emissions_path = "workflow/data/new_hector_emissions.csv",
                                old_emissions_constraint = "workflow/data/ssp119_emiss-constraints_rf.csv",
                                new_name = "gcam_emissions",
                                directory = "workflow/data")

write_input_file(emissions_constraint_file = "workflow/data/gcam_emissions_constraints.csv",
                 new_name = "gcam_emissions",
                 directory = "workflow/data")


# 3 using whole game function ---------------------------------------------

write_emissions_input(gcam_data = "workflow/data/gcam_emissions.dat",
                      old_emissions_constraint = "workflow/data/ssp119_emiss-constraints_rf.csv",
                      directory = "workflow/data")

# 4 Test run on Hector with plot --------------------------------------------

# read in the new ini file and run hector with it 
ini_file <- "workflow/data/gcam_emissions.ini" # update to whatever name of new file
core <- hector::newcore(ini_file)
hector::run(core, runtodate = 2100) #gcam_emissions.csv file only goes to 2005
out <- fetchvars(core, 1750:2100)
head(out)
reset(core)

ini_ssp119 <- system.file("input/hector_ssp119.ini", package = "hector")
core2 <- hector::newcore(ini_ssp119)
hector::run(core2, runtodate = 2100)
out2 <- fetchvars(core2, 1750:2100)
head(out2)

new_emissions <- 
  ggplot() + 
  geom_line(data = out, 
            aes(x = year, y = value),
            color = "blue") +
  facet_wrap(~variable, scales = "free")
new_emissions
new_emissions +
  geom_line(
    data = out2, 
    aes(x = year, y = value), 
    color = "red") +
  facet_wrap(~variable, scales = "free")


# Test run on Matilda with plot -----------------------------------------------------

params <- generate_params(core, 20)
m_result <- iterate_model(core, params, save_years = 1850:2100)

m_result2 <- iterate_model(core2, params, save_years = 1850:2100)

emissions_plot <- 
  ggplot() +
  geom_line(data = m_result,
            aes(x = year,
                y = value,
                group = run_number), 
            color = "blue") +
  facet_wrap(~variable, scales = "free")
emissions_plot +
  geom_line(data = m_result2,
            aes( x= year,
                 y = value,
                 group = run_number), 
            color = "red") +
  facet_wrap(~variable, scales = "free")