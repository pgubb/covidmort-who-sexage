---
title: "Age-sex patterns of COVID mortality across countries"
format: 
  html:
    keep-md: true
    toc: true
    toc-depth: 6
    toc-location: left
---





## Introduction

This paper seeks to understand how mortality risks from COVID for men and women differ by age, and how gender differences in mortality risk from COVID compare to gender differences in mortality risks from all-causes. 

## Data

This analyses in this paper use global excess deaths associated with COVID-19 (modeled estimates) produced by the [Technical Advisory Group (TAG) on COVID-19 mortality assessment](https://www.who.int/data/sets/global-excess-deaths-associated-with-covid-19-modelled-estimates). Estimates of expected all-cause deaths (baseline), actual cause deaths and excess deaths are provided by country and 7 age groups:"0-24" , "25-34" , 35-44", "45-54", "55-64", "65-74", "75-84", ">85". However, only age groups at or above 45 years of age are considered. 

Out of 195 countries in the WHO database, 75 countries have excess death estimates based on reported all-cause death records disaggregated by gender for 2020 and of these, 61 also use reported deaths by gender and age for 2021.

Country inclusion criteria for this analysis are as follows: 

- Have reported all-cause data disaggregated by age and sex in 2020 or 2021
- Have total excess deaths (for both sexes) of at least 2000 deaths in 2020 or 2021

- The following 65 countries are included in this analysis: Albania, Argentina, Australia, Austria, Azerbaijan, Belgium, Bulgaria, Bosnia and Herzegovina, Bolivia (Plurinational State of), Brazil, Canada, Switzerland, Chile, Colombia, Costa Rica, Cuba, Czechia, Germany, Dominican Republic, Ecuador, Egypt, Spain, Estonia, Finland, France, The United Kingdom, Georgia, Greece, Guatemala, Croatia, Hungary, Iran (Islamic Republic of), Iraq, Israel, Italy, Japan, Kazakhstan, Kyrgyzstan, Republic of Korea, Kuwait, Lithuania, Latvia, Republic of Moldova, Mexico, Mongolia, Nicaragua, Netherlands, Oman, Panama, Peru, Poland, Portugal, Paraguay, Romania, Russian Federation, Serbia, Slovakia, Sweden, Thailand, Tunisia, Ukraine, Uruguay, USA, Uzbekistan, South Africa
- These are the countries that meet the inclusion criteria in 2020: Albania, Argentina, Austria, Azerbaijan, Belgium, Bulgaria, Bosnia and Herzegovina, Bolivia (Plurinational State of), Brazil, Canada, Switzerland, Chile, Colombia, Czechia, Germany, Dominican Republic, Ecuador, Egypt, Spain, France, The United Kingdom, Georgia, Greece, Guatemala, Croatia, Hungary, Iran (Islamic Republic of), Iraq, Israel, Italy, Kazakhstan, Kyrgyzstan, Kuwait, Lithuania, Republic of Moldova, Mexico, Nicaragua, Netherlands, Oman, Panama, Peru, Poland, Portugal, Romania, Russian Federation, Serbia, Slovakia, Sweden, Thailand, Tunisia, Ukraine, USA, Uzbekistan, South Africa
- These are the countries that meet the inclusion criteria in 2021: Albania, Australia, Austria, Azerbaijan, Belgium, Bulgaria, Bolivia (Plurinational State of), Brazil, Canada, Switzerland, Chile, Colombia, Costa Rica, Cuba, Czechia, Germany, Dominican Republic, Ecuador, Spain, Estonia, Finland, France, The United Kingdom, Georgia, Greece, Guatemala, Croatia, Hungary, Iran (Islamic Republic of), Israel, Italy, Japan, Kazakhstan, Republic of Korea, Kuwait, Lithuania, Latvia, Republic of Moldova, Mexico, Mongolia, Netherlands, Oman, Peru, Poland, Portugal, Paraguay, Romania, Russian Federation, Serbia, Slovakia, Sweden, Thailand, Ukraine, Uruguay, USA, Uzbekistan, South Africa

#### Table 1: Sample of countries in analysis data-set by GNI per capita (PPP) quintiles

::: {.cell fig.asp='0.9'}
::: {.cell-output-display}
| Income quintile| No. of countries|Range of GNI per capita (PPP) in quintile |Countires in quintile                                                                                                                                                                 |
|---------------:|----------------:|:-----------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|               1|               13|5,000 - 13,510                            |Bolivia (Plurinational State of), Cuba, Ecuador, Egypt, Guatemala, Iraq, Kyrgyzstan, Mongolia, Nicaragua, Peru, Tunisia, Ukraine, Uzbekistan                                          |
|               2|               13|13,770 - 18,360                           |Albania, Azerbaijan, Bosnia and Herzegovina, Brazil, Colombia, Dominican Republic, Georgia, Iran (Islamic Republic of), Republic of Moldova, Paraguay, Serbia, Thailand, South Africa |
|               3|               13|19,630 - 32,600                           |Argentina, Bulgaria, Chile, Costa Rica, Greece, Croatia, Kazakhstan, Latvia, Mexico, Panama, Romania, Russian Federation, Uruguay                                                     |
|               4|               13|32,740 - 46,190                           |Czechia, Spain, Estonia, Hungary, Israel, Italy, Japan, Republic of Korea, Lithuania, Oman, Poland, Portugal, Slovakia                                                                |
|               5|               13|49,140 - 71,070                           |Australia, Austria, Belgium, Canada, Switzerland, Germany, Finland, France, The United Kingdom, Kuwait, Netherlands, Sweden, USA                                                      |
:::
:::


## Results

### Figure 1: Sex-ratio in mortality (all-cause and excess in 2020): Observations

::: {.cell fig.asp='0.9'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-3-1.png){width=1152}
:::
:::



### Figure 2: Poisson model results

::: {.cell fig.asp='1.4'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-4-1.png){width=1152}
:::
:::


### Relationship between predicted sex-differences in mortality and country income

#### Figure 3: Sex-ratio of mortality vs. country income 
All available data (composition of countries changes from 2020 to 2021)


::: {.cell fig.asp='0.818'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-5-1.png){width=1152}
:::
:::


#### Table 2: Sex ratio of mortality by age, year and country income quintiles

::: {.cell}
::: {.cell-output-display}
Table: Median and interquartile range of predictions are shown

|group                             | Age_Lower|Quintile: 1      |Quintile: 2      |Quintile: 3      |Quintile: 4      |Quintile: 5      |
|:---------------------------------|---------:|:----------------|:----------------|:----------------|:----------------|:----------------|
|All cause deaths (expected): 2020 |        45|1.75 (1.49-2.48) |2.44 (1.92-2.94) |3.05 (2.1-3.63)  |2.98 (2.3-3.74)  |2.16 (1.64-2.88) |
|All cause deaths (expected): 2020 |        55|1.66 (1.42-2.08) |2.03 (1.74-2.34) |2.38 (1.83-2.68) |2.34 (1.93-2.76) |1.82 (1.49-2.24) |
|All cause deaths (expected): 2020 |        65|1.58 (1.35-1.76) |1.69 (1.54-1.9)  |1.85 (1.61-1.99) |1.84 (1.61-2.05) |1.54 (1.35-1.76) |
|All cause deaths (expected): 2020 |        75|1.43 (1.26-1.59) |1.48 (1.31-1.63) |1.44 (1.33-1.53) |1.44 (1.33-1.56) |1.3 (1.18-1.41)  |
|All cause deaths (expected): 2020 |        85|1.25 (1.12-1.5)  |1.28 (1.09-1.43) |1.12 (1.03-1.25) |1.13 (1.04-1.23) |1.08 (0.95-1.19) |
|Excess deaths: 2020               |        45|2.13 (1.35-3.08) |3.12 (1.81-4.96) |2.27 (1.86-3.03) |4.86 (3-7.8)     |3.64 (2.29-6.34) |
|Excess deaths: 2020               |        55|2.18 (1.47-3)    |2.54 (1.68-3.5)  |2.23 (1.92-2.75) |3.7 (2.57-5.11)  |2.89 (1.95-4.43) |
|Excess deaths: 2020               |        65|2.18 (1.59-3)    |2.18 (1.72-2.71) |2.18 (1.92-2.6)  |2.79 (2.15-3.43) |2.33 (1.72-3.18) |
|Excess deaths: 2020               |        75|2.16 (1.6-2.97)  |1.89 (1.54-2.4)  |2.03 (1.81-2.48) |2.09 (1.68-2.47) |1.94 (1.52-2.46) |
|Excess deaths: 2020               |        85|2.12 (1.52-3.1)  |1.66 (1.22-2.41) |1.9 (1.6-2.53)   |1.57 (1.31-1.9)  |1.57 (1.16-2.04) |
|Excess deaths: 2021               |        45|1.78 (1.23-2.43) |1.69 (1.12-2.7)  |1.97 (1.53-2.75) |2.4 (1.66-3.22)  |2.24 (1.23-3.94) |
|Excess deaths: 2021               |        55|1.76 (1.2-2.18)  |1.54 (1.17-2.16) |1.85 (1.54-2.29) |2.25 (1.77-2.8)  |2.15 (1.37-3.36) |
|Excess deaths: 2021               |        65|1.66 (1.15-2)    |1.54 (1.27-1.87) |1.74 (1.5-2)     |2.1 (1.78-2.57)  |2.11 (1.54-3.18) |
|Excess deaths: 2021               |        75|1.48 (1.06-1.89) |1.54 (1.31-1.76) |1.56 (1.37-1.81) |2 (1.66-2.44)    |2.13 (1.42-3.42) |
|Excess deaths: 2021               |        85|1.34 (0.91-1.87) |1.5 (1.15-1.81)  |1.41 (1.18-1.73) |1.89 (1.42-2.6)  |2.07 (1.15-3.94) |
:::
:::


#### Figure 4: Sex-ratio of mortality vs. country income 
Keeping composition of countries constant (only countries with all cause deaths disaggregate by gender in 2020 & 2021)

::: {.cell fig.asp='0.818'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-7-1.png){width=1152}
:::
:::



## Annex 

### Observatios and predictions by country

The following set of figures display observed and predicted values of mortality rates, the sex-ratio of mortality rates (males/females) and the sex-gap of mortality rates (males - females) by age, for each country in the analysis dataset. 


::: {.cell fig.asp='0.818'}

#### Albania 
- Total population: 2,864,098 
- Total official COVID deaths in 2020 (Our World in Data): 1,170 
- Total official COVID deaths in 2021  (Our World in Data): 2,042 
- Total excess deaths in 2020  (WHO): 6,015.699 
- Total excess deaths in 2021  (WHO): 9,086.141 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-1.png){width=960}
:::


#### Argentina 
- Total population: 44,985,105 
- Total official COVID deaths in 2020 (Our World in Data): 48,271 
- Total official COVID deaths in 2021  (Our World in Data): 69,905 
- Total excess deaths in 2020  (WHO): 35,840.96 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-2.png){width=960}
:::


#### Australia 
- Total population: 25,625,670 
- Total official COVID deaths in 2020 (Our World in Data): 920 
- Total official COVID deaths in 2021  (Our World in Data): 1,523 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 4,570.856 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-3.png){width=960}
:::


#### Austria 
- Total population: 8,888,592 
- Total official COVID deaths in 2020 (Our World in Data): 7,379 
- Total official COVID deaths in 2021  (Our World in Data): 9,319 
- Total excess deaths in 2020  (WHO): 8,087.427 
- Total excess deaths in 2021  (WHO): 7,886.081 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-4.png){width=960}
:::


#### Azerbaijan 
- Total population: 10,282,158 
- Total official COVID deaths in 2020 (Our World in Data): 2,575 
- Total official COVID deaths in 2021  (Our World in Data): 5,771 
- Total excess deaths in 2020  (WHO): 12,039.15 
- Total excess deaths in 2021  (WHO): 21,581.59 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-5.png){width=960}
:::


#### Belgium 
- Total population: 11,533,459 
- Total official COVID deaths in 2020 (Our World in Data): 19,674 
- Total official COVID deaths in 2021  (Our World in Data): 8,682 
- Total excess deaths in 2020  (WHO): 16,540.23 
- Total excess deaths in 2021  (WHO): 3,794.365 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-6.png){width=960}
:::


#### Bulgaria 
- Total population: 6,967,018 
- Total official COVID deaths in 2020 (Our World in Data): 7,515 
- Total official COVID deaths in 2021  (Our World in Data): 23,375 
- Total excess deaths in 2020  (WHO): 17,804.05 
- Total excess deaths in 2021  (WHO): 42,715.42 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-7.png){width=960}
:::


#### Bosnia and Herzegovina 
- Total population: 3,314,350 
- Total official COVID deaths in 2020 (Our World in Data): 4,050 
- Total official COVID deaths in 2021  (Our World in Data): 9,378 
- Total excess deaths in 2020  (WHO): 6,792.844 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-8.png){width=960}
:::


#### Bolivia (Plurinational State of) 
- Total population: 11,934,333 
- Total official COVID deaths in 2020 (Our World in Data): 9,135 
- Total official COVID deaths in 2021  (Our World in Data): 10,515 
- Total excess deaths in 2020  (WHO): 33,888.56 
- Total excess deaths in 2021  (WHO): 47,020.77 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-9.png){width=960}
:::


#### Brazil 
- Total population: 213,114,918 
- Total official COVID deaths in 2020 (Our World in Data): 192,681 
- Total official COVID deaths in 2021  (Our World in Data): 426,136 
- Total excess deaths in 2020  (WHO): 189,945.8 
- Total excess deaths in 2021  (WHO): 467,480.1 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-10.png){width=960}
:::


#### Canada 
- Total population: 37,818,806 
- Total official COVID deaths in 2020 (Our World in Data): 15,274 
- Total official COVID deaths in 2021  (Our World in Data): 14,684 
- Total excess deaths in 2020  (WHO): 14,551.54 
- Total excess deaths in 2021  (WHO): 11,949.19 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-11.png){width=960}
:::


#### Switzerland 
- Total population: 8,619,390 
- Total official COVID deaths in 2020 (Our World in Data): 7,528 
- Total official COVID deaths in 2021  (Our World in Data): 4,393 
- Total excess deaths in 2020  (WHO): 8,808.004 
- Total excess deaths in 2021  (WHO): 3,683.951 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-12.png){width=960}
:::


#### Chile 
- Total population: 19,276,612 
- Total official COVID deaths in 2020 (Our World in Data): 16,499 
- Total official COVID deaths in 2021  (Our World in Data): 22,597 
- Total excess deaths in 2020  (WHO): 14,771.02 
- Total excess deaths in 2021  (WHO): 23,894.96 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-13.png){width=960}
:::


#### Colombia 
- Total population: 50,911,158 
- Total official COVID deaths in 2020 (Our World in Data): 42,620 
- Total official COVID deaths in 2021  (Our World in Data): 87,246 
- Total excess deaths in 2020  (WHO): 53,886.16 
- Total excess deaths in 2021  (WHO): 108,332.4 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-14.png){width=960}
:::


#### Costa Rica 
- Total population: 5,119,855 
- Total official COVID deaths in 2020 (Our World in Data): 2,156 
- Total official COVID deaths in 2021  (Our World in Data): 5,198 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 4,704.336 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-15.png){width=960}
:::


#### Cuba 
- Total population: 11,284,757 
- Total official COVID deaths in 2020 (Our World in Data): 145 
- Total official COVID deaths in 2021  (Our World in Data): 8,177 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 53,096.88 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-16.png){width=960}
:::


#### Czechia 
- Total population: 10,514,740 
- Total official COVID deaths in 2020 (Our World in Data): 11,888 
- Total official COVID deaths in 2021  (Our World in Data): 24,444 
- Total excess deaths in 2020  (WHO): 16,599.66 
- Total excess deaths in 2021  (WHO): 26,648.28 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-17.png){width=960}
:::


#### Germany 
- Total population: 83,116,786 
- Total official COVID deaths in 2020 (Our World in Data): 50,447 
- Total official COVID deaths in 2021  (Our World in Data): 68,023 
- Total excess deaths in 2020  (WHO): 34,514.19 
- Total excess deaths in 2021  (WHO): 66,990.79 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-18.png){width=960}
:::


#### Dominican Republic 
- Total population: 10,994,170 
- Total official COVID deaths in 2020 (Our World in Data): 2,409 
- Total official COVID deaths in 2021  (Our World in Data): 1,837 
- Total excess deaths in 2020  (WHO): 2,333.078 
- Total excess deaths in 2021  (WHO): 6,689.009 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-19.png){width=960}
:::


#### Ecuador 
- Total population: 17,580,085 
- Total official COVID deaths in 2020 (Our World in Data): 14,023 
- Total official COVID deaths in 2021  (Our World in Data): 19,646 
- Total excess deaths in 2020  (WHO): 46,706.66 
- Total excess deaths in 2021  (WHO): 32,874.78 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-20.png){width=960}
:::


#### Egypt 
- Total population: 107,450,137 
- Total official COVID deaths in 2020 (Our World in Data): 7,520 
- Total official COVID deaths in 2021  (Our World in Data): 14,175 
- Total excess deaths in 2020  (WHO): 107,524 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-21.png){width=960}
:::


#### Spain 
- Total population: 47,233,572 
- Total official COVID deaths in 2020 (Our World in Data): 54,459 
- Total official COVID deaths in 2021  (Our World in Data): 37,229 
- Total excess deaths in 2020  (WHO): 75,201.66 
- Total excess deaths in 2021  (WHO): 32,336.08 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-22.png){width=960}
:::


#### Estonia 
- Total population: 1,326,400 
- Total official COVID deaths in 2020 (Our World in Data): 229 
- Total official COVID deaths in 2021  (Our World in Data): 1,703 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 3,037.613 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-23.png){width=960}
:::


#### Finland 
- Total population: 5,516,103 
- Total official COVID deaths in 2020 (Our World in Data): 587 
- Total official COVID deaths in 2021  (Our World in Data): 1,118 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 2,868.022 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-24.png){width=960}
:::


#### France 
- Total population: 64,291,311 
- Total official COVID deaths in 2020 (Our World in Data): 64,004 
- Total official COVID deaths in 2021  (Our World in Data): 56,958 
- Total excess deaths in 2020  (WHO): 50,200.12 
- Total excess deaths in 2021  (WHO): 37,445.36 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-25.png){width=960}
:::


#### The United Kingdom 
- Total population: 66,921,778 
- Total official COVID deaths in 2020 (Our World in Data): 93,317 
- Total official COVID deaths in 2021  (Our World in Data): 83,496 
- Total excess deaths in 2020  (WHO): 81,734.67 
- Total excess deaths in 2021  (WHO): 65,350.31 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-26.png){width=960}
:::


#### Georgia 
- Total population: 3,762,236 
- Total official COVID deaths in 2020 (Our World in Data): 2,505 
- Total official COVID deaths in 2021  (Our World in Data): 11,295 
- Total excess deaths in 2020  (WHO): 5,738.193 
- Total excess deaths in 2021  (WHO): 17,255.76 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-27.png){width=960}
:::


#### Greece 
- Total population: 10,484,757 
- Total official COVID deaths in 2020 (Our World in Data): 4,788 
- Total official COVID deaths in 2021  (Our World in Data): 15,920 
- Total excess deaths in 2020  (WHO): 7,277.952 
- Total excess deaths in 2021  (WHO): 19,485.75 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-28.png){width=960}
:::


#### Guatemala 
- Total population: 17,355,773 
- Total official COVID deaths in 2020 (Our World in Data): 4,803 
- Total official COVID deaths in 2021  (Our World in Data): 11,299 
- Total excess deaths in 2020  (WHO): 12,580.13 
- Total excess deaths in 2021  (WHO): 36,476.57 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-29.png){width=960}
:::


#### Croatia 
- Total population: 4,089,053 
- Total official COVID deaths in 2020 (Our World in Data): 3,860 
- Total official COVID deaths in 2021  (Our World in Data): 8,633 
- Total excess deaths in 2020  (WHO): 5,870.759 
- Total excess deaths in 2021  (WHO): 12,268.29 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-30.png){width=960}
:::


#### Hungary 
- Total population: 9,733,129 
- Total official COVID deaths in 2020 (Our World in Data): 9,537 
- Total official COVID deaths in 2021  (Our World in Data): 29,649 
- Total excess deaths in 2020  (WHO): 12,046.75 
- Total excess deaths in 2021  (WHO): 26,972.14 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-31.png){width=960}
:::


#### Iran (Islamic Republic of) 
- Total population: 87,266,417 
- Total official COVID deaths in 2020 (Our World in Data): 54,946 
- Total official COVID deaths in 2021  (Our World in Data): 76,581 
- Total excess deaths in 2020  (WHO): 136,293.9 
- Total excess deaths in 2021  (WHO): 159,238.4 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-32.png){width=960}
:::


#### Iraq 
- Total population: 42,553,401 
- Total official COVID deaths in 2020 (Our World in Data): 12,800 
- Total official COVID deaths in 2021  (Our World in Data): 11,351 
- Total excess deaths in 2020  (WHO): 28,125.58 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-33.png){width=960}
:::


#### Israel 
- Total population: 8,746,610 
- Total official COVID deaths in 2020 (Our World in Data): 3,336 
- Total official COVID deaths in 2021  (Our World in Data): 4,923 
- Total excess deaths in 2020  (WHO): 3,177.944 
- Total excess deaths in 2021  (WHO): 4,795.487 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-34.png){width=960}
:::


#### Italy 
- Total population: 59,314,503 
- Total official COVID deaths in 2020 (Our World in Data): 73,604 
- Total official COVID deaths in 2021  (Our World in Data): 63,643 
- Total excess deaths in 2020  (WHO): 109,157.4 
- Total excess deaths in 2021  (WHO): 75,106.86 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-35.png){width=960}
:::


#### Japan 
- Total population: 124,706,807 
- Total official COVID deaths in 2020 (Our World in Data): 3,414 
- Total official COVID deaths in 2021  (Our World in Data): 14,979 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 22,117.4 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-36.png){width=960}
:::


#### Kazakhstan 
- Total population: 18,973,597 
- Total official COVID deaths in 2020 (Our World in Data): 2,749 
- Total official COVID deaths in 2021  (Our World in Data): 15,478 
- Total excess deaths in 2020  (WHO): 31,218.43 
- Total excess deaths in 2021  (WHO): 52,050.25 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-37.png){width=960}
:::


#### Kyrgyzstan 
- Total population: 6,423,515 
- Total official COVID deaths in 2020 (Our World in Data): 1,355 
- Total official COVID deaths in 2021  (Our World in Data): 1,447 
- Total excess deaths in 2020  (WHO): 7,420.808 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-38.png){width=960}
:::


#### Republic of Korea 
- Total population: 51,776,733 
- Total official COVID deaths in 2020 (Our World in Data): 900 
- Total official COVID deaths in 2021  (Our World in Data): 4,725 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 8,229.939 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-39.png){width=960}
:::


#### Kuwait 
- Total population: 4,360,120 
- Total official COVID deaths in 2020 (Our World in Data): 932 
- Total official COVID deaths in 2021  (Our World in Data): 1,536 
- Total excess deaths in 2020  (WHO): 3,203.663 
- Total excess deaths in 2021  (WHO): 3,344.758 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-40.png){width=960}
:::


#### Lithuania 
- Total population: 2,813,500 
- Total official COVID deaths in 2020 (Our World in Data): 1,799 
- Total official COVID deaths in 2021  (Our World in Data): 5,597 
- Total excess deaths in 2020  (WHO): 6,154.16 
- Total excess deaths in 2021  (WHO): 11,292.63 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-41.png){width=960}
:::


#### Latvia 
- Total population: 1,892,761 
- Total official COVID deaths in 2020 (Our World in Data): 635 
- Total official COVID deaths in 2021  (Our World in Data): 3,935 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 6,846.926 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-42.png){width=960}
:::


#### Republic of Moldova 
- Total population: 3,082,703 
- Total official COVID deaths in 2020 (Our World in Data): 3,110 
- Total official COVID deaths in 2021  (Our World in Data): 7,159 
- Total excess deaths in 2020  (WHO): 5,436.351 
- Total excess deaths in 2021  (WHO): 10,905.46 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-43.png){width=960}
:::


#### Mexico 
- Total population: 125,917,904 
- Total official COVID deaths in 2020 (Our World in Data): 147,623 
- Total official COVID deaths in 2021  (Our World in Data): 156,199 
- Total excess deaths in 2020  (WHO): 297,154 
- Total excess deaths in 2021  (WHO): 315,216 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-44.png){width=960}
:::


#### Mongolia 
- Total population: 3,293,544 
- Total official COVID deaths in 2020 (Our World in Data): 1 
- Total official COVID deaths in 2021  (Our World in Data): 1,985 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 2,599.867 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-45.png){width=960}
:::


#### Nicaragua 
- Total population: 6,754,777 
- Total official COVID deaths in 2020 (Our World in Data): 165 
- Total official COVID deaths in 2021  (Our World in Data): 52 
- Total excess deaths in 2020  (WHO): 8,118.323 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-46.png){width=960}
:::


#### Netherlands 
- Total population: 17,403,110 
- Total official COVID deaths in 2020 (Our World in Data): 11,296 
- Total official COVID deaths in 2021  (Our World in Data): 9,589 
- Total excess deaths in 2020  (WHO): 14,530.01 
- Total excess deaths in 2021  (WHO): 16,001.69 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-47.png){width=960}
:::


#### Oman 
- Total population: 4,542,924 
- Total official COVID deaths in 2020 (Our World in Data): 1,497 
- Total official COVID deaths in 2021  (Our World in Data): 2,986 
- Total excess deaths in 2020  (WHO): 2,534.465 
- Total excess deaths in 2021  (WHO): 5,942.41 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-48.png){width=960}
:::


#### Panama 
- Total population: 4,291,548 
- Total official COVID deaths in 2020 (Our World in Data): 3,933 
- Total official COVID deaths in 2021  (Our World in Data): 3,492 
- Total excess deaths in 2020  (WHO): 2,861.276 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-49.png){width=960}
:::


#### Peru 
- Total population: 33,290,096 
- Total official COVID deaths in 2020 (Our World in Data): 93,066 
- Total official COVID deaths in 2021  (Our World in Data): 109,518 
- Total excess deaths in 2020  (WHO): 100,106.6 
- Total excess deaths in 2021  (WHO): 124,084.2 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-50.png){width=960}
:::


#### Poland 
- Total population: 38,358,520 
- Total official COVID deaths in 2020 (Our World in Data): 28,642 
- Total official COVID deaths in 2021  (Our World in Data): 68,416 
- Total excess deaths in 2020  (WHO): 63,032.16 
- Total excess deaths in 2021  (WHO): 100,112.2 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-51.png){width=960}
:::


#### Portugal 
- Total population: 10,274,559 
- Total official COVID deaths in 2020 (Our World in Data): 6,842 
- Total official COVID deaths in 2021  (Our World in Data): 12,095 
- Total excess deaths in 2020  (WHO): 11,245.03 
- Total excess deaths in 2021  (WHO): 12,245.21 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-52.png){width=960}
:::


#### Paraguay 
- Total population: 6,615,894 
- Total official COVID deaths in 2020 (Our World in Data): 2,220 
- Total official COVID deaths in 2021  (Our World in Data): 14,404 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 18,371.62 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-53.png){width=960}
:::


#### Romania 
- Total population: 19,411,764 
- Total official COVID deaths in 2020 (Our World in Data): 15,596 
- Total official COVID deaths in 2021  (Our World in Data): 43,118 
- Total excess deaths in 2020  (WHO): 37,164.14 
- Total excess deaths in 2021  (WHO): 76,235.53 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-54.png){width=960}
:::


#### Russian Federation 
- Total population: 145,420,443 
- Total official COVID deaths in 2020 (Our World in Data): 57,019 
- Total official COVID deaths in 2021  (Our World in Data): 251,841 
- Total excess deaths in 2020  (WHO): 380,136.8 
- Total excess deaths in 2021  (WHO): 713,027.8 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-55.png){width=960}
:::


#### Serbia 
- Total population: 7,347,278 
- Total official COVID deaths in 2020 (Our World in Data): 3,163 
- Total official COVID deaths in 2021  (Our World in Data): 9,525 
- Total excess deaths in 2020  (WHO): 16,724.08 
- Total excess deaths in 2021  (WHO): 36,442.32 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-56.png){width=960}
:::


#### Slovakia 
- Total population: 5,449,592 
- Total official COVID deaths in 2020 (Our World in Data): 2,138 
- Total official COVID deaths in 2021  (Our World in Data): 14,497 
- Total excess deaths in 2020  (WHO): 5,849.537 
- Total excess deaths in 2021  (WHO): 20,291.01 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-57.png){width=960}
:::


#### Sweden 
- Total population: 10,347,118 
- Total official COVID deaths in 2020 (Our World in Data): 9,706 
- Total official COVID deaths in 2021  (Our World in Data): 5,639 
- Total excess deaths in 2020  (WHO): 8,932.776 
- Total excess deaths in 2021  (WHO): 3,273.921 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-58.png){width=960}
:::


#### Thailand 
- Total population: 71,392,182 
- Total official COVID deaths in 2020 (Our World in Data): 61 
- Total official COVID deaths in 2021  (Our World in Data): 21,637 
- Total excess deaths in 2020  (WHO): 7,530.558 
- Total excess deaths in 2021  (WHO): 66,204.06 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-59.png){width=960}
:::


#### Tunisia 
- Total population: 12,156,148 
- Total official COVID deaths in 2020 (Our World in Data): 4,620 
- Total official COVID deaths in 2021  (Our World in Data): 20,944 
- Total excess deaths in 2020  (WHO): 4,322.457 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-60.png){width=960}
:::


#### Ukraine 
- Total population: 43,843,015 
- Total official COVID deaths in 2020 (Our World in Data): 18,533 
- Total official COVID deaths in 2021  (Our World in Data): 77,366 
- Total excess deaths in 2020  (WHO): 43,125.64 
- Total excess deaths in 2021  (WHO): 144,710.5 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-61.png){width=960}
:::


#### Uruguay 
- Total population: 3,422,352 
- Total official COVID deaths in 2020 (Our World in Data): 168 
- Total official COVID deaths in 2021  (Our World in Data): 6,000 
- Total excess deaths in 2020  (WHO):  
- Total excess deaths in 2021  (WHO): 5,906.342 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-62.png){width=960}
:::


#### USA 
- Total population: 335,410,796 
- Total official COVID deaths in 2020 (Our World in Data): 352,004 
- Total official COVID deaths in 2021  (Our World in Data): 467,051 
- Total excess deaths in 2020  (WHO): 467,312.8 
- Total excess deaths in 2021  (WHO): 501,112.5 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-63.png){width=960}
:::


#### Uzbekistan 
- Total population: 33,520,434 
- Total official COVID deaths in 2020 (Our World in Data): 614 
- Total official COVID deaths in 2021  (Our World in Data): 871 
- Total excess deaths in 2020  (WHO): 15,794.15 
- Total excess deaths in 2021  (WHO): 14,826.1 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-64.png){width=960}
:::


#### South Africa 
- Total population: 58,778,516 
- Total official COVID deaths in 2020 (Our World in Data): 28,033 
- Total official COVID deaths in 2021  (Our World in Data): 63,028 
- Total excess deaths in 2020  (WHO): 56,967.63 
- Total excess deaths in 2021  (WHO): 189,834.3 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-8-65.png){width=960}
:::
:::


<!-- ### Poisson regression coefficients  -->

<!-- #### Unadjusted model -->
<!-- ```{r fig.width = 10, fig.asp = 1.5} -->
<!-- #| echo: false -->
<!-- #| warning: false -->

<!-- visualize_coeffs(model_results, models = "1. Unadjusted model (Deaths ~ a + b*male)") -->

<!-- ``` -->

<!-- #### Age-adjusted model (no interaction) -->
<!-- ```{r fig.width = 10, fig.asp = 1.5} -->
<!-- #| echo: false -->
<!-- #| warning: false -->

<!-- visualize_coeffs(model_results, models = "2. Adjusted model (Deaths ~ a + b*male + c*age)") -->

<!-- ``` -->


<!-- ### Relationship between sex-differences in mortality and country income -->

<!-- #### Mortality rates -->

<!-- ##### Observations -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->
<!-- #| warning: false -->

<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: Each point represents the observed mortality rate for men and women for each country/age-group combination. Observed excess mortality rates less than 0 are recoded to 1. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->

<!-- plot_data_v_income(obs_to_plot, slct_metric = "Mortality rate (per 100,000) [log-scale]", group_data = NULL, income_data = income, caption) -->

<!-- ``` -->

<!-- ##### Predictions -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->
<!-- #| warning: false -->
<!-- #|  -->
<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: Each point represents the median predicted mortality rate for men and women at each age for a single country from a simulation of 1,000 parameter draws from a Poisson model Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->

<!-- plot_data_v_income(simsummary_to_plot, slct_metric = "Mortality rate (per 100,000) [log-scale]", income_data = income, group_data = NULL, caption) -->

<!-- ``` -->

<!-- #### Sex-ratio in mortality rates -->

<!-- ##### Observations -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->

<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: In cases where negative excess mortality rates are less than 0 for men or women, those values are recoded to 1. Observed sex ratios greater than 20 are recoded to 20. Each point represents the observed relative risk of mortality between men and women for a single country/age-group combination. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->

<!-- plot_data_v_income(obs_to_plot, slct_metric = "Relative risk (Males/Females)", income_data = income, group_data = NULL, caption) -->


<!-- ``` -->

<!-- ##### Predictions -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->

<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: Each point represents the median predicted relative risk of mortality between men and women for a single country from a simulation of 1,000 parameter draws from a Poisson model. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->

<!-- plot_data_v_income(simsummary_to_plot, slct_metric = "Relative risk (Males/Females)", income_data = income, group_data = NULL, caption) -->


<!-- ``` -->

<!-- #### Sex-gap in mortality rates -->

<!-- ##### Observations -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->

<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: Each point represents the observed difference in mortality between men and women for a single country/age-group. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->
<!-- plot_data_v_income(obs_to_plot, slct_metric = "Absolute risk difference (Males - Females)", income_data = income, group_data = NULL, caption) -->

<!-- ``` -->

<!-- ##### Predictions -->
<!-- ```{r fig.width = 10, fig.asp = 0.818} -->
<!-- #| echo: false -->

<!-- caption <- str_wrap( -->
<!--   paste0("Source: Author's calculations using WHO excess death estimates by sex.  -->
<!--          Notes: Each point represents the median predicted risk difference of mortality between men and women for a single country/age-group from a simulation of 1,000 parameter draws from a Poisson model. Only countries with actual all cause-deaths in 2020 available for both men and women by age are used."),  -->
<!--   115) -->
<!-- plot_data_v_income(simsummary_to_plot, slct_metric = "Absolute risk difference (Males - Females)", income_data = income, group_data = NULL, caption) -->

<!-- ``` -->

