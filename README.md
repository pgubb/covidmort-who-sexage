# covidmort-who-sexage
Examining how COVID mortality rates vary by sex and age using WHO excess mortality estimates

**Description**

This repository contains all of the data and code necessary to reproduce the results in the paper: "COVID-19 Increased Existing Gender Mortality Gaps in High Income More than Middle Income Countries".

**Data and Code Availability Statement**

All of the data used in this analysis is available publicly from the WHO, specifically in the files produced by the [Technical Advisory Group (TAG) on COVID-19 mortality assessment](https://www.who.int/data/sets/global-excess-deaths-associated-with-covid-19-modelled-estimates). One additional data file is used for this analysis which lists the set of countries in 2020 and 2021 for which **actual** deaths by gender and age (found here: *Data/who_data_bysex_2023.csv*) This .csv file was manually constructed from the table in the TAG's [methodology document](https://www.who.int/publications/m/item/methods-for-estimating-the-excess-mortality-associatedwith-the-covid-19-pandemic) on pps. 22-23. 

UPDATE: As of Late 2023, after an update, the WHO TAG data files no longer include population estimates by country, sex and age, so rather than downloading the latest data from the WHO website as defined in the *get_raw_data function*, the *get_raw_data_fromfile* function is used to retrieve that data originally used for the study, stored in the WHO_COVID_Excess_Deaths_Estimates folder. 

**Computational requirements**
This analysis was conducted using R/version 4.2.3 (2023-03-15) and RStudio/version 2023.03.0+386 (2023.03.0+386). 

sessionInfo()
R version 4.2.3 (2023-03-15)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Ventura 13.3.1

**Instructions for Data Preparation and Analysis**
The full workflow for data downloading, cleaning, analysis and visuazliation of results is set out in the file: *covidmort-who-sexage-results.qmd*. This is a quarto file which when run, generates as output the *covidmort-who-sexage-results.md* and *covidmort-who-sexage.html* files. The individual functions which perform the data downloading, data cleaning, data analysis and data visualization tasks are in the R subfolder. 

