
##########################################
# Downloading WHO Excess mortality dataset --------
##########################################

library(tidyverse)
library(lubridate)
library(WDI)
library(clarify)
library(viridis)
library(ggtext)
library(readxl)
library(broom)
library(glue)
library(scales)
library(kableExtra)

'%not_in%' <- function(x,y)!('%in%'(x,y))

# file <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/covid-19-excessmortality/2022-03-25_covid-19_gem.zip?sfvrsn=2d5835e0_2"
#LINK_TO_WHO_DATA <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/covid-19-excessmortality/2023-05-19_covid-19_gem.zip?sfvrsn=9a95fc1a_1"

LINK_TO_WHO_DATA <- "https://cdn.who.int/media/docs/default-source/world-health-data-platform/covid-19-excessmortality/2023-05-19_covid-19_gem.zip?sfvrsn=9a95fc1a_3"
AGE_TO_CENTER <- 65

######################################################
# Downloading Auxiliary data: Our World in Data Total Cumulative Official COVID deaths by country and year -----
######################################################

owid <- function(url = "https://covid.ourworldindata.org/data/owid-covid-data.csv") {

  # Function that downloads OWID data and returns df with total cumulative "Official" COVID deaths by country and year

  read_csv(url, guess_max = 25000) %>%
    filter(!is.na(total_cases)) %>%
    rename(iso3c = iso_code) %>%
    mutate(year = year(date)) %>%
    filter(year %in% c(2020, 2021)) %>%
    arrange(iso3c, year, date) %>%
    group_by(iso3c, year) %>%
    filter(row_number() == n()) %>%
    select(year, cum_covid_deaths = total_deaths) %>%
    pivot_wider(values_from = starts_with("cum_"), names_from = "year", names_prefix = "cum_covid_deaths_") %>%
    mutate(tot_covid_deaths_2021 = cum_covid_deaths_2021 - cum_covid_deaths_2020)

}

######################################################
# Downloading Auxiliary data: GNI per capita -------
######################################################

wdi <- function(indicator = c("gni_pcap_ppp" = "NY.GNP.PCAP.PP.CD")) {

  # Manually adding GNI per capita data for Taiwan and Czechia
  addons <- tibble(
    iso3c = c("TWN", "CZE"),
    gni_pcap_ppp = c(55078,39220),
    income = c(rep(c("High income"), 2))
  )

  # Downloaidng GNI per capita (PPP) data from WDI (World Bank)
  WDI(country = "all", indicator = indicator, extra = TRUE, start = 2019, end = 2019) %>%
    mutate(iso3c = as.character(iso3c)) %>%
    select(iso3c, iso2c, gni_pcap_ppp, income) %>%
    filter(income != "Aggregates") %>%
    mutate(gni_pcap_ppp = ifelse(iso3c == "CUB", 8920, gni_pcap_ppp)) %>%
    bind_rows(addons) %>%
    select(-iso2c)

}

###############################################
# Downloading WHO Excess death dataset  -------
###############################################

get_raw_data <- function(url = LINK_TO_WHO_DATA,
                         sheet = "Deaths by year, sex and age") {

  # The WHO excess mortality datasets are downloadable a 3 excel files in a zipped folder
  # This function downloads the folder and extracts the relevant excel file

  temp <- tempfile() # Creating temporary filepath for download
  download.file(url, temp)
  files <- unzip(temp, list = TRUE) # unzipping folder

  # The target file is WHO_COVID_Excess_Deaths_EstimatesByCountry.xlsx
  target_file <- unzip(temp, files[[1]][2])
  sprintf("Opening %s: ", target_file)
  sheets <- excel_sheets(target_file)

  read_excel(target_file, sheet = sheet, skip = 10)

}

get_raw_data_fromfile <- function(file = "WHO_COVID_Excess_Deaths_Estimates/WHO_COVID_Excess_Deaths_EstimatesByCountry.xlsx", sheet =  "Deaths by year, sex and age") {

  read_excel(file, sheet = sheet, skip = 11)

}

##########################################
# Cleaning WHO Excess mortality dataset ------
##########################################

clean_raw_data_old <- function(raw_data) {

  # Adding and renaming columns

  raw_data %>%
    filter(!is.na(iso3)) %>%  # Removes notes at the bottom of the file
    mutate(
      Age_Lower = case_when(
        age == "0 to 24" ~ 0,
        age == "25 to 39" ~ 25,
        age == "40 to 49" ~ 40,
        age == "50 to 59" ~ 50,
        age == "60 to 69" ~ 60,
        age == "70 to 79" ~ 70,
        age == "80 plus" ~ 80
      ),
      Sex = ifelse(sex == "Female", "f", "m"),
      sex = ifelse(sex == "Female", "Females", "Males"),
      country = ifelse(country == "United States of America", "USA", country),
      data_source = "WHO",
      Year_fct = factor(year, levels = c("2020", "2021"), ordered = TRUE)
    ) %>%
    rename(Country = country, Age_Int = age, Sex_name = sex, Year = year, iso3c = iso3)

}

clean_raw_data <- function(raw_data) {

  # Adding and renaming columns

  raw_data %>%
    filter(!is.na(iso3)) %>%  # Removes notes at the bottom of the file
    mutate(
      Age_Lower = case_when(
        age_group == "0-24" ~ 0,
        age_group == "25-34" ~ 25,
        age_group == "35-44" ~ 35,
        age_group == "45-54" ~ 45,
        age_group == "55-64" ~ 55,
        age_group == "65-74" ~ 65,
        age_group == "75-84" ~ 75,
        age_group == ">85" ~ 85
      ),
      Sex = ifelse(sex == "Female", "f", "m"),
      sex = ifelse(sex == "Female", "Females", "Males"),
      country = ifelse(country == "United States of America", "USA", country),
      data_source = "WHO",
      Year_fct = factor(year, levels = c("2020", "2021"), ordered = TRUE)
    ) %>%
    rename(Country = country, Age_Int = age_group, Sex_name = sex, Year = year, iso3c = iso3, Nx = pop, excess.mean = `excess.mean*`)

  }

###############################################
# Creating an auxiliary file which identifies the type of death data underpinning WHO calculations -------
###############################################

# Identifying which countries rely on "reported" death data vs. "predicted" death data by year

get_data_types <- function(clean_data) {

  # This functoin returns a df which identifies which countries rely on "reported" death data vs. "predicted" death data by year

  datatype_all <- clean_data %>%
    select(iso3c, Country, Year, type) %>%
    group_by(iso3c, Country, Year) %>%
    filter(row_number() == 1) %>%
    pivot_wider(names_from = "Year", values_from = "type", names_prefix = "datatype_all_") %>%
    mutate(mismatch = ifelse(datatype_all_2020 != datatype_all_2021, 1, 0)) %>%
    mutate(
      datatype_all = ifelse(mismatch == 0, datatype_all_2021, "Mixed")
    ) %>%
    ungroup() %>%
    select(iso3c, datatype_all_2020, datatype_all_2021, datatype_all)

  # Additional identification for countries with reproted all-cause daeth data both by sex and age
  datatype_bysex <- read_csv("./data/who_data_bysex_2023.csv") %>%
                      select(iso3c, Deaths_2020, Deaths_2021) %>%
                      mutate(
                        Deaths_2020 = as.integer(str_trim(Deaths_2020)),
                        Deaths_2021 = as.integer(str_trim(Deaths_2021))
                      )

  datatype_all %>%
    left_join(datatype_bysex, by = c("iso3c")) %>%
    mutate(
      datatype_gender_2020 = ifelse(!is.na(Deaths_2020), "Reported", "Predicted"),
      datatype_gender_2021 = ifelse(!is.na(Deaths_2021), "Reported", "Predicted")
    ) %>%
    select(-Deaths_2020, -Deaths_2021) %>%
    mutate(
      reported_all_2020 = ifelse(datatype_all_2020 == "Reported", 1, 0),
      reported_all_2021 = ifelse(datatype_all_2021 == "Reported", 1, 0),
      reported_all_both = ifelse(reported_all_2020 == 1 & reported_all_2021 == 1, 1, 0),
      predicted_all_2020 = ifelse(datatype_all_2020 == "Predicted", 1, 0),
      predicted_all_2021 = ifelse(datatype_all_2021 == "Predicted", 1, 0),
      reported_gender_2020 = ifelse(datatype_gender_2020 == "Reported", 1, 0),
      reported_gender_2021 = ifelse(datatype_gender_2021 == "Reported", 1, 0),
      reported_gender_both = ifelse(reported_gender_2020 == 1 & reported_gender_2021 == 1, 1, 0),
      predicted_gender_2020 = ifelse(datatype_gender_2020 == "Predicted", 1, 0),
      predicted_gender_2021 = ifelse(datatype_gender_2021 == "Predicted", 1, 0),
      N = 1,
      segment = case_when(
        reported_gender_2020 == 0 & reported_gender_2021 == 0 & reported_all_2020 == 0 & reported_all_2021 == 0 ~ "5. ACD predicted (2020 & 2021)",
        reported_gender_2020 == 0 & reported_gender_2021 == 0 & reported_all_2020 == 1 & reported_all_2021 == 0 ~ "4. ACD reported for both sexes combined (2020 only)",
        reported_gender_2020 == 0 & reported_gender_2021 == 0 & reported_all_2020 == 1 & reported_all_2021 == 1 ~ "3. ACD reported for both sexes combined (2020 & 2021)",
        reported_gender_2020 == 1 & reported_gender_2021 == 0 ~ "2. ACD reported by gender (2020 only)",
        reported_gender_both == 1 ~ "1. ACD reported by gender (2020 & 2021)"
      )
    )

}

get_pop <- function(clean_data) {

  # Getting population by country
  clean_data %>%
    filter(Year == "2020") %>%
    group_by(iso3c) %>%
    # Adding population across age and sex
    summarize(
      NxTOT = sum(Nx)
    ) %>%
    select(iso3c, NxTOT)

}

get_countrytoiso <- function(clean_data) {

  clean_data %>%
    group_by(iso3c, Country) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(iso3c, Country)

  }

######################################################
# Processing WHO Excess mortality dataset for analysis ------
######################################################

prep_analysis_data <- function(clean_data, country_income, covid_deaths, data_types, params) {

  # Creating a df with the 2020 total population (both sexes combined)
  pop <- clean_data %>%
    filter(Year == "2020") %>%
    group_by(iso3c) %>%
    # Adding population across age and sex
    summarize(
      NxTOT = sum(Nx)
    ) %>%
    ungroup() %>%
    select(iso3c, NxTOT)

  # Creating a df with the male+female excess deaths for 2020 and 2021
  xdtot_wide <- clean_data %>%
    #filter(Age_Lower >= params[["AGE_THRESHOLD"]]) %>%
    group_by(iso3c, Year) %>%
    # Adding population across age and sex
    summarize(
      excess_deaths_total = sum(excess.mean)
    ) %>%
    pivot_wider(values_from = excess_deaths_total, names_from = Year, names_prefix = "excess_deaths_total_") %>%
    ungroup() %>%
    select(iso3c, excess_deaths_total_2020, excess_deaths_total_2021)

  xdtot_long <- clean_data %>%
    #filter(Age_Lower >= params[["AGE_THRESHOLD"]]) %>%
    group_by(iso3c, Year) %>%
    # Adding population across age and sex
    summarize(
      excess_deaths_total = sum(excess.mean)
    ) %>%
    select(iso3c, Year, excess_deaths_total)

  # This function creates a "long" version of the WHO dataset, with type of death distinguished by the column "source"

  clean_data %>%
    mutate(
      Deaths = excess.mean,
      # Mortality rate
      mr = (Deaths/Nx)*1e5,
      source = "Excess deaths"
    ) -> deaths_excess

  clean_data %>%
    mutate(
      Deaths = expected.mean,
      # Mortality rate
      mr = (Deaths/Nx)*1e5,
      source = "All-cause deaths (expected)"
    ) -> deaths_allcauses

  clean_data %>%
    mutate(
      Deaths = acm.mean,
      # Mortality rate
      mr = (Deaths/Nx)*1e5,
      source = "All-cause deaths (actual)"
    ) -> deaths_allcauses_actual

  # Combining them together

  bind_rows(deaths_excess, deaths_allcauses, deaths_allcauses_actual) %>%
      # Incorporating total population data
      left_join(pop, by = "iso3c") %>%
      left_join(xdtot_wide, by = c("iso3c")) %>%
      left_join(xdtot_long, by = c("iso3c", "Year")) %>%
      left_join(country_income, by = "iso3c") %>%
      left_join(covid_deaths, by = "iso3c") %>%
      left_join(data_types, by = "iso3c") %>%
      mutate(
        excess_deaths_total = ifelse(source == "All-cause deaths (expected)", 999999, excess_deaths_total),
        keepobs_reported = ifelse((reported_gender_2020 == params[["USE_ACTUALS_BYSX_2020"]] & Year == 2020) |
                           (reported_gender_2021 == params[["USE_ACTUALS_BYSX_2021"]] & Year == 2021), 1, 0)
      ) %>%

      # Applying inclusion criteria
      filter(Age_Lower >= params[["AGE_THRESHOLD"]]) %>%
      filter(keepobs_reported == 1) %>%
      filter(excess_deaths_total_2020 >= params[["DEATHS_THRESHOLD"]] | excess_deaths_total_2021 >= params[["DEATHS_THRESHOLD"]]) %>%
      filter(excess_deaths_total >= params[["DEATHS_THRESHOLD"]]) %>%
      #filter(NxTOT >= params[["POP_THRESHOLD"]]) %>%
      filter(iso3c %not_in% params[["COUNTRY_EXCLUSIONS"]]) %>%

      # Creating predictor variables for Poisson regression
      mutate(
        female = ifelse(Sex == "f", 1, 0),
        male = ifelse(Sex == "m", 1, 0),
        Age_Lower_rec = Age_Lower - AGE_TO_CENTER,
        Age_Lower_rec = Age_Lower_rec/10,
        Deaths_poiss = ifelse(Deaths <= 0, 0, Deaths)
      )

  }

######################################################
# Function that fits a Poisson model and simulates predictions to create uncertainty bounds ------
######################################################

simulate <- function(analysis_data, sel_iso, sel_source, sel_year, n = 1000) {

  df <- analysis_data %>% filter(iso3c == sel_iso & source == sel_source & Year == sel_year)

  fit <- glm(Deaths_poiss ~ male + Age_Lower_rec + male*Age_Lower_rec, data = df, family = quasipoisson, offset = log(Nx))

  # Running clarify to get contrasts between each level in the access strand

  set.seed(1234)
  sim_coefs <- sim(fit, n = n)

  N_AGE_GROUPS = length(unique(analysis_data$Age_Int))

  # Prediction dataset
  df_p <- df %>% ungroup() %>% select(male, Age_Lower_rec)

  df_p <- do.call("rbind", replicate(n, df_p, simplify = FALSE)) %>% mutate(sim = rep(1:n, each = N_AGE_GROUPS*2))

  sims <- as_tibble(sim_coefs$sim.coefs) %>% mutate(sim = row_number())
  names(sims) <- c("cb", "cm", "ca", "cm_a", "sim")

  sims <- sims %>%
    left_join(df_p, by = "sim") %>%
    #predictions
    mutate(
      mrp = exp(cb + cm*male + ca*Age_Lower_rec + cm_a*male*Age_Lower_rec)*1e5
    )

  # Computing Relative Risk of male:female mortality and Absolute risk male - female

  sims %>%
    pivot_wider(names_from = male, values_from = mrp, names_prefix = "males_") %>%
    mutate(
      RRmf = males_1/males_0,
      ARmf = males_1 - males_0,
      iso3c = sel_iso,
      source = sel_source,
      Year = sel_year
    ) -> sims

  return(sims)

}

######################################################
# Function that applies simulation function to country-level data
######################################################

apply_simulate <- function(analysis_data) {

# Preparing data for analysis ----------------
iso_to_country <- analysis_data %>% group_by(iso3c) %>% filter(row_number() == 1) %>% select(iso3c, Country)

analysis_data <- analysis_data %>% mutate(source_iso_year = paste(source, iso3c, Year, sep = "_"))

sources <- unique(analysis_data$source)
countries <- unique(analysis_data$iso3c)
years <- unique(analysis_data$Year)

#params <- expand.grid(sources, countries, years, stringsAsFactors = FALSE)

params <- analysis_data %>% select(source_iso_year) %>% separate(source_iso_year, into = c("sel_source", "sel_iso", "sel_year"), sep = "_")

#names(params) <- c("sel_source", "sel_iso", "sel_year")

params %>%
  mutate(drop = ifelse(sel_source == "All-cause deaths (expected)" & sel_year == 2021, 1, 0)) %>%
  filter(drop == 0) %>% select(-drop) -> params

# Running simulation and returning results ----------------
 dplyr::bind_rows(
  pmap(params,
       simulate,
       analysis_data = analysis_data,
       n = 1000)
) %>%
  mutate(
    Age_Lower = Age_Lower_rec*10 + AGE_TO_CENTER,
    sim_source_year = paste(sim, source, Year, sep ="_"),
    source_year = paste(source, Year, sep = ": "),
    iso_sim = paste(iso3c, sim)
  ) %>%
  left_join(iso_to_country, by = c("iso3c"))

}

######################################################
# Function that creates summary statistics from the simulations and prepares a visualization dataset
######################################################

prep_sims_summary <- function(sims) {

  sims_long <- sims %>%
    #filter(year != 2021) %>%
    pivot_longer(cols = starts_with("males_"), values_to = "mr", names_prefix = "males_" , names_to = "sex") %>%
    mutate(
      Sex_name = ifelse(sex == 1, "Males", "Females"),
      Sex = ifelse(sex == 1, "m", "f")
    ) %>% select(-sex)

  sumstats <- function(data, var, metric, type, ...) {

    var <- enquo(var)

    data %>%
      group_by(...) %>%
      mutate(
        !!var := ifelse(is.infinite(!!var) | is.nan(!!var), NA, !!var),
        p5 = quantile(!!var, prob = 0.05, na.rm = TRUE),
        p25 = quantile(!!var, prob = 0.25, na.rm = TRUE),
        p50 = quantile(!!var, prob = 0.5, na.rm = TRUE),
        p75 = quantile(!!var, prob = 0.75, na.rm = TRUE),
        p95 = quantile(!!var, prob = 0.95, na.rm = TRUE),
        mean = mean(!!var),
        metric = metric,
        type = type
      ) %>%
      filter(row_number() == 1) %>%
      ungroup() -> summary

    if (quo_name(var) %in% c("RRmf", "ARmf")) {
      summary %>% mutate(
        Sex_name = "Not applicable"
      ) -> summary
    }

    return(summary %>% select(iso3c, Country, source, Year, Age_Lower, Sex_name, metric, type, p5, p25, p50, p75, p95, mean))

  }

  # Summary stats of mortality rates across simulations by country, sex and age
  sims_sum_mr <- sumstats(sims_long, mr, metric = "Mortality rate (per 100,000) [log-scale]", type = "Prediction", iso3c, source, Year, Sex_name, Age_Lower, Age_Lower_rec)

  # Summary stats of relative risk (M/F) across simulations, by country and age
  sims_sum_rr <- sumstats(sims, RRmf, metric = "Relative risk (Males/Females)", type = "Prediction", iso3c, source, Year, Age_Lower, Age_Lower_rec)

  # Summary stats of absolute risk (M-F) across simulations, by country and age
  sims_sum_ar <- sumstats(sims, ARmf, metric = "Absolute risk difference (Males - Females)", type = "Prediction", iso3c, source, Year, Age_Lower, Age_Lower_rec)

  # Compiling into single dataset
  rbind(sims_sum_mr, sims_sum_rr, sims_sum_ar) %>%
    mutate(source_year = paste(source, Year, sep = ": "),
           source_year_sex = paste(source_year, Sex_name, sep = ": "),
           metric = factor(metric, levels = c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), ordered = TRUE),
           metric_lab = factor(str_wrap(metric, 20), levels = str_wrap(c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), 20), ordered = TRUE),
           logp50 = ifelse(metric %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p50, log(p50)),
           logp5 = ifelse(metric  %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p5,  log(p5)),
           logp95 = ifelse(metric %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p95, log(p95))
    )

  }


prep_sims_summary_by_inc_quint <- function(sims, income) {

  sims <- sims %>%
    left_join(income, by = "iso3c") %>%
    mutate(
      gnipcap_quintiles = cut(gni_pcap_ppp, breaks = quantile(gni_pcap_ppp, probs = seq(0, 1, 1/3)), label = FALSE, ordered_result = TRUE, include.lowest = TRUE)
    )

  sims_long <- sims %>%
    #filter(year != 2021) %>%
    pivot_longer(cols = starts_with("males_"), values_to = "mr", names_prefix = "males_" , names_to = "sex") %>%
    mutate(
      Sex_name = ifelse(sex == 1, "Males", "Females"),
      Sex = ifelse(sex == 1, "m", "f")
    ) %>% select(-sex)

  sumstats <- function(data, var, metric, type, ...) {

    var <- enquo(var)

    data %>%
      group_by(...) %>%
      mutate(
        !!var := ifelse(is.infinite(!!var) | is.nan(!!var), NA, !!var),
        p5 = quantile(!!var, prob = 0.05, na.rm = TRUE),
        p25 = quantile(!!var, prob = 0.25, na.rm = TRUE),
        p50 = quantile(!!var, prob = 0.5, na.rm = TRUE),
        p75 = quantile(!!var, prob = 0.75, na.rm = TRUE),
        p95 = quantile(!!var, prob = 0.95, na.rm = TRUE),
        mean = mean(!!var),
        metric = metric,
        type = type
      ) %>%
      filter(row_number() == 1) %>%
      ungroup() -> summary

    if (quo_name(var) %in% c("RRmf", "ARmf")) {
      summary %>% mutate(
        Sex_name = "Not applicable"
      ) -> summary

      return(summary %>% select(...,  Sex_name, metric, type, p5, p25, p50, p75, p95, mean))

    } else {
      return(summary %>% select(...,  metric, type, p5, p25, p50, p75, p95, mean))
    }

  }

  # Summary stats of mortality rates across simulations by country, sex and age
  sims_sum_mr <- sumstats(sims_long, mr, metric = "Mortality rate (per 100,000) [log-scale]", type = "Prediction", gnipcap_quintiles, source, Year, Sex_name, Age_Lower, Age_Lower_rec)

  # Summary stats of relative risk (M/F) across simulations, by country and age
  sims_sum_rr <- sumstats(sims, RRmf, metric = "Relative risk (Males/Females)", type = "Prediction", gnipcap_quintiles, source, Year, Age_Lower, Age_Lower_rec)

  # Summary stats of absolute risk (M-F) across simulations, by country and age
  sims_sum_ar <- sumstats(sims, ARmf, metric = "Absolute risk difference (Males - Females)", type = "Prediction", gnipcap_quintiles, source, Year, Age_Lower, Age_Lower_rec)

  # Compiling into single dataset
  rbind(sims_sum_mr, sims_sum_rr, sims_sum_ar) %>%
    mutate(source_year = paste(source, Year, sep = ": "),
           source_year_sex = paste(source_year, Sex_name, sep = ": "),
           metric = factor(metric, levels = c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), ordered = TRUE),
           metric_lab = factor(str_wrap(metric, 20), levels = str_wrap(c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), 20), ordered = TRUE),
           logp50 = ifelse(metric %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p50, log(p50)),
           logp5 = ifelse(metric  %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p5,  log(p5)),
           logp95 = ifelse(metric %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), p95, log(p95))
    )

}


prep_results_table <- function(sims_summary, slct_metric, slct_source) {

  sims_summary %>%
    filter(metric == slct_metric & source %in% slct_source) %>%
    select(source, Year, Age_Lower, gnipcap_quintiles, p25, p50, p75) %>%
    arrange(gnipcap_quintiles, Age_Lower) %>%
    mutate(
      gnipcap_quintiles = paste0("Quintile: ", gnipcap_quintiles),
      group = paste(source, Year, sep = ": "),
      summary_stat = paste0(round(p50,2), " (", round(p25, 2), "-", round(p75, 2), ")")
    ) %>%
    pivot_wider(
      id_cols = c(group, Age_Lower),
      names_from = gnipcap_quintiles,
      values_from = summary_stat
    )
}

sample_summary_table <- function(analysis_data) {

  analysis_data %>%
    group_by(iso3c) %>%
    filter(row_number() == 1) %>%
    select(Country, gni_pcap_ppp) %>%
    ungroup() %>%
    mutate(gnipcap_quintiles = cut(gni_pcap_ppp, breaks = quantile(gni_pcap_ppp, probs = seq(0, 1, 1/3)), label = FALSE, ordered_result = TRUE, include.lowest = TRUE),
           N = 1) %>%
    group_by(gnipcap_quintiles) %>%
    summarize(
      N = sum(N),
      min = prettyNum(min(gni_pcap_ppp), big.mark=","),
      max = prettyNum(max(gni_pcap_ppp), big.mark = ","),
      min_max = paste(min, max, sep = " - "),
      countries = reduce(Country, paste, sep= ", ")
    ) %>%
    select(gnipcap_quintiles, N, min_max, countries) %>%
    rename(`Income quintile` = gnipcap_quintiles, `No. of countries` = N, `Range of GNI per capita (PPP) in quintile` = min_max, `Countires in quintile` = countries)

}

######################################################
# Function that prepares actual observations
######################################################

prep_obs_data <- function(analysis_data) {

# Actual observations -----------

obs_mr <- analysis_data %>%
  mutate(metric = "Mortality rate (per 100,000) [log-scale]",
         type = "Observation",
         source_year = paste(source, Year, sep = ": "),
         value_adj = ifelse(mr <= 0, 1, mr)) %>%
  filter(source_year %not_in% c("All-cause deaths (expected): 2021")) %>%
  rename(value = mr) %>%
  select(iso3c, Country, source, Year, Age_Lower, Sex_name, metric, type, value, value_adj)

obs_wide <- obs_mr %>%
  pivot_wider(id_cols = c("iso3c", "Country", "source", "Year", "Age_Lower"),  values_from = "value", names_from = "Sex_name")

obs_rr <- obs_wide %>%
  mutate(value = Males/Females,
         value_adj = ifelse(Males <= 0, 1, Males)/ifelse(Females <= 0, 1, Females),
         value_adj = ifelse(value_adj > 20, 20, value_adj),
         metric = "Relative risk (Males/Females)",
         type = "Observation",
         Sex_name = "Not applicable") %>%
        select(-Males, -Females)

obs_ar <- obs_wide %>%
  mutate(value = Males - Females,
         value_adj = NA,
         metric = "Absolute risk difference (Males - Females)",
         type = "Observation",
         Sex_name = "Not applicable") %>%
  select(-Males, -Females)

obs_all <- rbind(obs_mr, obs_rr, obs_ar) %>%
  mutate(source_year = paste(source, Year, sep = ": "),
         source_year_sex = paste(source_year, Sex_name, sep = ": "),
         metric = factor(metric, levels = c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), ordered = TRUE),
         metric_lab = factor(str_wrap(metric, 20), levels = str_wrap(c("Mortality rate (per 100,000) [log-scale]", "Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), 20), ordered = TRUE),
         absvl = abs(value),
         logval = log(absvl),
         neg_val = ifelse(value < 0, -1, 1),
         logval = logval*neg_val,
         logval = ifelse(is.infinite(logval), 0, logval),
         logval = ifelse(metric %in% c("Relative risk (Males/Females)", "Absolute risk difference (Males - Females)"), value, logval)
  )

}


####################################
# Function that generates a comparison of three regression models
###################################

apply_model <- function(df, model, model_no, model_label) {

  df %>%
    group_by(source, Year, iso3c) %>%
    nest() %>%
    mutate(
      model = map(data, model),
      results = map(model, tidy)
    ) %>%
    select(Year, iso3c, results) %>%
    unnest(cols = c(results)) %>%
    ungroup() %>%
    mutate(
      term_label = case_when(
        term == "(Intercept)" ~ "Female mortality rate per 100,000",
        term == "male" ~ "Ratio of male-to-female mortality",
        term == "Age_Lower_rec" ~ "Age-slope of female mortality",
        term == "male:Age_Lower_rec" ~ "Marginal effect of male on age-slope",
      ),
      term_label2 = case_when(
        term == "(Intercept)" ~ "Female mortality rate per 100,000",
        term == "male" ~ "Ratio of male-to-female mortality",
        term == "Age_Lower_rec" ~ "Age-slope of female mortality",
        term == "male:Age_Lower_rec" ~ "Age-slope of male mortality",
      ),
      term_label3 = case_when(
        term == "(Intercept)" ~ "Female mortality rate per 100,000 (1)",
        term == "male" ~ "Ratio of male-to-female mortality (2)",
        term == "Age_Lower_rec" ~ "Age-slope of female mortality (3)",
        term == "male:Age_Lower_rec" ~ "Age-slope of male mortality (4)",
      ),
      term_short = case_when(
        term == "(Intercept)" ~ "a1",
        term == "male" ~ "a2",
        term == "Age_Lower_rec" ~ "b1",
        term == "male:Age_Lower_rec" ~ "b2"
      ),
      term_label2 = factor(str_wrap(term_label2, 12),
                           levels = str_wrap(c("Female mortality rate per 100,000", "Ratio of male-to-female mortality", "Age-slope of female mortality", "Age-slope of male mortality"), 12), ordered = TRUE),
      term_label3 = factor(str_wrap(term_label3, 12),
                           levels = str_wrap(c("Female mortality rate per 100,000 (1)", "Ratio of male-to-female mortality (2)", "Age-slope of female mortality (3)", "Age-slope of male mortality (4)"), 12), ordered = TRUE),
      model_no = model_no,
      model_label = model_label,
      estimate = ifelse(exp(estimate) > 5000, NA, estimate),
      estimate_interp = exp(estimate),
      estimate_interp = ifelse(term == "(Intercept)", estimate_interp*1e5, estimate_interp)
    )  %>%
    group_by(source, Year, iso3c) %>%
    mutate(
      x = sum(ifelse(term_short %in% c("b1", "b2"), estimate, NA), na.rm = TRUE),
      x = ifelse(term_short %in% c("b2"), x, estimate),
      estimate_interp2 = exp(x),
      estimate_interp2 = ifelse(term == "(Intercept)", estimate_interp2*1e5, estimate_interp2)
    ) %>%
    select(-x)

}

####################################
# Function that stacks the results from apply_model
###################################

stack_models <- function(analysis_data) {

  model1 <- apply_model(
    analysis_data,
    function(df) {
      glm(Deaths_poiss ~ male, data = df, family = quasipoisson, offset = log(Nx))
    },
    model_no = 1,
    model_label = "1. Unadjusted model (Deaths ~ a + b*male)"
  )


  model2 <- apply_model(
    analysis_data,
    function(df) {
      glm(Deaths_poiss ~ male + Age_Lower_rec, data = df, family = quasipoisson, offset = log(Nx))
    },
    model_no = 2,
    model_label = "2. Adjusted model (Deaths ~ a + b*male + c*age)"
  )


  model3 <- apply_model(
    analysis_data,
    function(df) {
      glm(Deaths_poiss ~ male + Age_Lower_rec + male*Age_Lower_rec, data = df, family = quasipoisson, offset = log(Nx))
    },
    model_no = 3,
    model_label = "3. Adjusted model (Deaths ~ a + b*male + c*age + d*male*age)"
  )

  bind_rows(model1, model2, model3)

}



