# 8 Test run on Hector with plot --------------------------------------------

# read in the new ini file and run hector with it 
ini_file <- "workflow/data/new_emissions_constraint.ini" # update to whatever name of new file
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