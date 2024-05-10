ini_path <- "data/emissions_input.ini"

# Set up the Hector core
core1 <- newcore(ini_path, name = unique(hector_emissions_data$scenario))

# Run with gcam_emissions ini
run(core, runtodate = 2050)
out <- fetchvars(core = core, dates = 2005:2050, vars = c(GMST(), LUC_EMISSIONS()))




setvar(core = core, 
       dates = gcam_emissions_df$year, 
       var = gcam_emissions_df$variable, 
       values = gcam_emissions_df$value, 
       unit = gcam_emissions_df$units)
reset(core)

setvar(core = core, 
       dates = luc_emissions$year, 
       var = luc_emissions$variable, 
       values = luc_emissions$value, 
       unit = luc_emissions$units)
reset(core)

fetchvars(core, 2005, vars = LUC_EMISSIONS())

luc_uptake <- data.frame(year = 2005:2100,
                         var = LUC_UPTAKE(), 
                         values = 0, 
                         unit = getunits(FFI_EMISSIONS()))

setvar(core = core, 
       dates = luc_uptake$year,
       var = LUC_UPTAKE(), 
       values = 0, 
       unit = getunits(FFI_EMISSIONS()))

fetchvars(core, 1745, vars = LUC_UPTAKE())

run(core, runtodate = 2050)
out <- fetchvars(core = core, dates = 2005:2050, vars = c(GMST(), LUC_EMISSIONS()))

test_run <- run(core = core_test, runtodate = 2100)
