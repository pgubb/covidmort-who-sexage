
##########################################
# DEFINING a list that contains inclusion criteria for the analysis dataset ------
##########################################

params <- list(
  "AGE_THRESHOLD" = 40, # Only include age-groups at ar above this age
  "POP_THRESHOLD" = 1e6, # Only include countries with total population at or above this threshold
  "DEATHS_THRESHOLD" = 2000, # Only include countries with total cumulative official COVID deaths at end of 2020 at or above this threshold, 
  "USE_ACTUALS_BYSX_2020" = 1,  # Use cases where actual deaths are the basis for ED estiamtes for males and females in 2020
  "USE_ACTUALS_BYSX_2021" = 1, # Use cases where actual deaths are the basis for ED estimates for males and females in 2021
  "COUNTRY_EXCLUSIONS" = c("URY", "TUN") # Country-specific exclusions
)

############
# DATA PREP
############

# Downloading Auxiliary data: Our World in Data Total Cumulative Official COVID deaths by country and year -----
deaths_official <- owid()

# Downloading Auxiliary data: GNI per capita -------
income <- wdi()

# Downloading WHO Excess death dataset  -------
raw_data <- get_raw_data()

# Cleaning WHO Excess mortality dataset ------
clean_data <- clean_raw_data(raw_data)

# Creating an auxiliary file which identifies the type of death data underpinning WHO calculations -------
who_data_types <- get_data_types(clean_data) 

# Processing WHO Excess mortality dataset for analysis ------
analysis_data <- prep_analysis_data(clean_data, income, deaths_official, who_data_types, params)


############
# RESULTS
############

simulate(analysis_data, sel_iso = "USA", sel_source = "Excess deaths", sel_year = "2020", n = 1000)

# Running model and simulation -------------
sims <- apply_simulate(analysis_data)

# Creating summary statistics from the simulations and prepares a visualization dataset ---------
simsummary_to_plot <- prep_sims_summary(sims)

# Preparing actual observations ---------
obs_to_plot <- prep_obs_data(analysis_data)


############
# VISUALIZING RESULTS
############

# Creating a figure for each country -------
selections <- unique(simsummary_to_plot$iso3c)
map(selections, plot_obs_and_preds, predictions = simsummary_to_plot, observations = obs_to_plot)


# High level results

caption <- str_wrap(
  paste0("Source: Author's calculations using WHO excess death estimates by sex. 
         Notes: Each point represents the median predicted mortality rate for men and women for a single country from a simulation of 1,000 parameter draws from a Poisson model. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."), 
  125)

plot_data_v_income(simsummary_to_plot, slct_metric = "Mortality rate (per 100,000) [log-scale]", income_data = income, caption)


caption <- str_wrap(
  paste0("Source: Author's calculations using WHO excess death estimates by sex. 
         Notes: Each point represents the median predicted relative risk of mortality between men and women for a single country from a simulation of 1,000 parameter draws from a Poisson model. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."), 
  125)

plot_data_v_income(simsummary_to_plot, slct_metric = "Relative risk (Males/Females)", income_data = income, caption)

caption <- str_wrap(
  paste0("Source: Author's calculations using WHO excess death estimates by sex. 
         Notes: Each point represents the median predicted risk difference of mortality between men and women for a single country from a simulation of 1,000 parameter draws from a Poisson model. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."), 
  125)
plot_data_v_income(simsummary_to_plot, slct_metric = "Absolute risk difference (Males - Females)", income_data = income, caption)
