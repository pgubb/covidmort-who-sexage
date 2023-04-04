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

This analyses in this paper use global excess deaths associated with COVID-19 (modeled estimates) produced by the [Technical Advisory Group (TAG) on COVID-19 mortality assessment](https://www.who.int/data/sets/global-excess-deaths-associated-with-covid-19-modelled-estimates). Estimates of expected all-cause deaths (baseline), actual cause deaths and excess deaths are provided by country and 7 age groups: 0-25, 25-39, 40-49, 50-59, 60-69, 70-79 and 80+. 

Out of 194 countries in the WHO database, only 35 countries (18 percent, mostly from Europe) have excess death estimates based on reported all-cause death records disaggregated by gender for both 2020 and 2021. 53 countries have reported all-cause death data for both sexes combined for both 2020 and 2021.  For nearly all lower -middle- and low-income countries, all-cause deaths are predicted. 

Inclusion criteria for this analysis are as follows: 

- Age groups at or above 40 years of age
- Countries with a total population of at least 1m 
- Countries with a cumulative official COVID-19 death toll of at least 2000 deaths at the end of 2020
- Countries with reported all-cause data disaggregated by age and sex in 2020
- The following 36 countries are included in this analysis: Austria, Belgium, Bulgaria, Brazil, Switzerland, Chile, Colombia, Czechia, Germany, Ecuador, Spain, France, The United Kingdom, Greece, Croatia, Hungary, Ireland, Iran (Islamic Republic of), Iraq, Israel, Italy, Japan, Kazakhstan, Mexico, Netherlands, Peru, Poland, Romania, Russian Federation, Serbia, Slovakia, Slovenia, Sweden, Ukraine, USA, South Africa

## Results

### Sex-ratio in mortality (all-cause and excess in 2020): Observations

::: {.cell fig.asp='0.9'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-2-1.png){width=1152}
:::
:::



### Age-adjusted poisson model (with interaction)

::: {.cell fig.asp='1.4'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-3-1.png){width=1152}
:::
:::


### Relationship between predicted sex-differences in mortality and country income

#### All available data (composition of countries changes in 2021)


::: {.cell fig.asp='0.818'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-4-1.png){width=1152}
:::
:::



#### Keeping composition of countries constant (only countries with all cause deaths disaggregate by gender in 2020 & 2021)

::: {.cell fig.asp='0.818'}
::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-5-1.png){width=1152}
:::
:::



## Annex 

### By country

The following set of figures display observed and predicted values of mortality rates, the sex-ratio of mortality rates (males/females) and the sex-gap of mortality rates (males - females) by age, for each country in the analysis dataset. 


::: {.cell fig.asp='0.818'}

#### Austria 
- Total population: 9,006,400 
- Total official COVID deaths in 2020 (Our World in Data): 7,380 
- Total official COVID deaths in 2021  (Our World in Data): 9,303 
- Total excess deaths in 2020  (WHO): 6,536 
- Total excess deaths in 2021  (WHO): 5,405 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-1.png){width=960}
:::


#### Belgium 
- Total population: 11,589,616 
- Total official COVID deaths in 2020 (Our World in Data): 19,674 
- Total official COVID deaths in 2021  (Our World in Data): 8,682 
- Total excess deaths in 2020  (WHO): 16,968 
- Total excess deaths in 2021  (WHO): 951 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-2.png){width=960}
:::


#### Bulgaria 
- Total population: 6,948,445 
- Total official COVID deaths in 2020 (Our World in Data): 7,515 
- Total official COVID deaths in 2021  (Our World in Data): 23,375 
- Total excess deaths in 2020  (WHO): 16,880 
- Total excess deaths in 2021  (WHO): 40,615 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-3.png){width=960}
:::


#### Brazil 
- Total population: 212,559,409 
- Total official COVID deaths in 2020 (Our World in Data): 192,681 
- Total official COVID deaths in 2021  (Our World in Data): 426,136 
- Total excess deaths in 2020  (WHO): 210,810 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-4.png){width=960}
:::


#### Switzerland 
- Total population: 8,654,618 
- Total official COVID deaths in 2020 (Our World in Data): 7,528 
- Total official COVID deaths in 2021  (Our World in Data): 4,393 
- Total excess deaths in 2020  (WHO): 7,540 
- Total excess deaths in 2021  (WHO): 707 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-5.png){width=960}
:::


#### Chile 
- Total population: 19,116,209 
- Total official COVID deaths in 2020 (Our World in Data): 16,499 
- Total official COVID deaths in 2021  (Our World in Data): 22,597 
- Total excess deaths in 2020  (WHO): 14,577 
- Total excess deaths in 2021  (WHO): 24,121 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-6.png){width=960}
:::


#### Colombia 
- Total population: 50,882,884 
- Total official COVID deaths in 2020 (Our World in Data): 42,620 
- Total official COVID deaths in 2021  (Our World in Data): 87,246 
- Total excess deaths in 2020  (WHO): 54,062 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-7.png){width=960}
:::


#### Czechia 
- Total population: 10,708,982 
- Total official COVID deaths in 2020 (Our World in Data): 11,888 
- Total official COVID deaths in 2021  (Our World in Data): 24,443 
- Total excess deaths in 2020  (WHO): 13,450 
- Total excess deaths in 2021  (WHO): 23,590 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-8.png){width=960}
:::


#### Germany 
- Total population: 83,783,945 
- Total official COVID deaths in 2020 (Our World in Data): 50,442 
- Total official COVID deaths in 2021  (Our World in Data): 67,991 
- Total excess deaths in 2020  (WHO): 66,793 
- Total excess deaths in 2021  (WHO): 128,194 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-9.png){width=960}
:::


#### Ecuador 
- Total population: 17,643,060 
- Total official COVID deaths in 2020 (Our World in Data): 14,023 
- Total official COVID deaths in 2021  (Our World in Data): 19,646 
- Total excess deaths in 2020  (WHO): 46,402 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-10.png){width=960}
:::


#### Spain 
- Total population: 46,754,783 
- Total official COVID deaths in 2020 (Our World in Data): 54,459 
- Total official COVID deaths in 2021  (Our World in Data): 37,229 
- Total excess deaths in 2020  (WHO): 72,574 
- Total excess deaths in 2021  (WHO): 31,361 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-11.png){width=960}
:::


#### France 
- Total population: 65,273,512 
- Total official COVID deaths in 2020 (Our World in Data): 64,004 
- Total official COVID deaths in 2021  (Our World in Data): 56,958 
- Total excess deaths in 2020  (WHO): 49,178 
- Total excess deaths in 2021  (WHO): 32,671 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-12.png){width=960}
:::


#### The United Kingdom 
- Total population: 67,886,004 
- Total official COVID deaths in 2020 (Our World in Data): 75,239 
- Total official COVID deaths in 2021  (Our World in Data): 74,688 
- Total excess deaths in 2020  (WHO): 85,504 
- Total excess deaths in 2021  (WHO): 63,392 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-13.png){width=960}
:::


#### Greece 
- Total population: 10,423,056 
- Total official COVID deaths in 2020 (Our World in Data): 4,788 
- Total official COVID deaths in 2021  (Our World in Data): 15,920 
- Total excess deaths in 2020  (WHO): 4,043 
- Total excess deaths in 2021  (WHO): 15,351 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-14.png){width=960}
:::


#### Croatia 
- Total population: 4,105,268 
- Total official COVID deaths in 2020 (Our World in Data): 3,860 
- Total official COVID deaths in 2021  (Our World in Data): 8,633 
- Total excess deaths in 2020  (WHO): 5,502 
- Total excess deaths in 2021  (WHO): 11,676 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-15.png){width=960}
:::


#### Hungary 
- Total population: 9,660,350 
- Total official COVID deaths in 2020 (Our World in Data): 9,537 
- Total official COVID deaths in 2021  (Our World in Data): 29,649 
- Total excess deaths in 2020  (WHO): 11,072 
- Total excess deaths in 2021  (WHO): 25,427 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-16.png){width=960}
:::


#### Ireland 
- Total population: 4,937,796 
- Total official COVID deaths in 2020 (Our World in Data): 2,264 
- Total official COVID deaths in 2021  (Our World in Data): 3,824 
- Total excess deaths in 2020  (WHO): 420 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-17.png){width=960}
:::


#### Iran (Islamic Republic of) 
- Total population: 83,992,953 
- Total official COVID deaths in 2020 (Our World in Data): 55,095 
- Total official COVID deaths in 2021  (Our World in Data): 76,477 
- Total excess deaths in 2020  (WHO): 108,918 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-18.png){width=960}
:::


#### Iraq 
- Total population: 40,222,503 
- Total official COVID deaths in 2020 (Our World in Data): 12,808 
- Total official COVID deaths in 2021  (Our World in Data): 11,346 
- Total excess deaths in 2020  (WHO): 40,399 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-19.png){width=960}
:::


#### Israel 
- Total population: 8,655,541 
- Total official COVID deaths in 2020 (Our World in Data): 3,336 
- Total official COVID deaths in 2021  (Our World in Data): 4,923 
- Total excess deaths in 2020  (WHO): 2,434 
- Total excess deaths in 2021  (WHO): 3,744 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-20.png){width=960}
:::


#### Italy 
- Total population: 60,461,828 
- Total official COVID deaths in 2020 (Our World in Data): 73,604 
- Total official COVID deaths in 2021  (Our World in Data): 63,643 
- Total excess deaths in 2020  (WHO): 100,432 
- Total excess deaths in 2021  (WHO): 60,368 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-21.png){width=960}
:::


#### Japan 
- Total population: 126,476,458 
- Total official COVID deaths in 2020 (Our World in Data): 3,414 
- Total official COVID deaths in 2021  (Our World in Data): 14,979 
- Total excess deaths in 2020  (WHO): -30,138 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-22.png){width=960}
:::


#### Kazakhstan 
- Total population: 18,776,707 
- Total official COVID deaths in 2020 (Our World in Data): 2,749 
- Total official COVID deaths in 2021  (Our World in Data): 15,478 
- Total excess deaths in 2020  (WHO): 28,493 
- Total excess deaths in 2021  (WHO): 47,721 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-23.png){width=960}
:::


#### Mexico 
- Total population: 128,932,753 
- Total official COVID deaths in 2020 (Our World in Data): 147,623 
- Total official COVID deaths in 2021  (Our World in Data): 156,199 
- Total excess deaths in 2020  (WHO): 314,541 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-24.png){width=960}
:::


#### Netherlands 
- Total population: 17,134,873 
- Total official COVID deaths in 2020 (Our World in Data): 11,296 
- Total official COVID deaths in 2021  (Our World in Data): 9,589 
- Total excess deaths in 2020  (WHO): 14,482 
- Total excess deaths in 2021  (WHO): 14,731 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-25.png){width=960}
:::


#### Peru 
- Total population: 32,971,846 
- Total official COVID deaths in 2020 (Our World in Data): 93,066 
- Total official COVID deaths in 2021  (Our World in Data): 109,518 
- Total excess deaths in 2020  (WHO): 133,817 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-26.png){width=960}
:::


#### Poland 
- Total population: 37,846,605 
- Total official COVID deaths in 2020 (Our World in Data): 28,642 
- Total official COVID deaths in 2021  (Our World in Data): 68,416 
- Total excess deaths in 2020  (WHO): 60,687 
- Total excess deaths in 2021  (WHO): 96,844 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-27.png){width=960}
:::


#### Romania 
- Total population: 19,237,682 
- Total official COVID deaths in 2020 (Our World in Data): 15,596 
- Total official COVID deaths in 2021  (Our World in Data): 43,118 
- Total excess deaths in 2020  (WHO): 34,995 
- Total excess deaths in 2021  (WHO): 71,917 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-28.png){width=960}
:::


#### Russian Federation 
- Total population: 145,934,460 
- Total official COVID deaths in 2020 (Our World in Data): 57,019 
- Total official COVID deaths in 2021  (Our World in Data): 251,841 
- Total excess deaths in 2020  (WHO): 369,306 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-29.png){width=960}
:::


#### Serbia 
- Total population: 8,737,370 
- Total official COVID deaths in 2020 (Our World in Data): 3,163 
- Total official COVID deaths in 2021  (Our World in Data): 9,525 
- Total excess deaths in 2020  (WHO): 17,173 
- Total excess deaths in 2021  (WHO): 38,470 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-30.png){width=960}
:::


#### Slovakia 
- Total population: 5,459,643 
- Total official COVID deaths in 2020 (Our World in Data): 2,138 
- Total official COVID deaths in 2021  (Our World in Data): 14,497 
- Total excess deaths in 2020  (WHO): 5,463 
- Total excess deaths in 2021  (WHO): 18,857 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-31.png){width=960}
:::


#### Slovenia 
- Total population: 2,078,932 
- Total official COVID deaths in 2020 (Our World in Data): 2,984 
- Total official COVID deaths in 2021  (Our World in Data): 3,145 
- Total excess deaths in 2020  (WHO): 3,346 
- Total excess deaths in 2021  (WHO): 2,238 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-32.png){width=960}
:::


#### Sweden 
- Total population: 10,099,270 
- Total official COVID deaths in 2020 (Our World in Data): 9,616 
- Total official COVID deaths in 2021  (Our World in Data): 5,719 
- Total excess deaths in 2020  (WHO): 8,502 
- Total excess deaths in 2021  (WHO): 2,751 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-33.png){width=960}
:::


#### Ukraine 
- Total population: 43,733,759 
- Total official COVID deaths in 2020 (Our World in Data): 18,533 
- Total official COVID deaths in 2021  (Our World in Data): 77,366 
- Total excess deaths in 2020  (WHO): 45,366 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-34.png){width=960}
:::


#### USA 
- Total population: 331,002,647 
- Total official COVID deaths in 2020 (Our World in Data): 352,004 
- Total official COVID deaths in 2021  (Our World in Data): 467,051 
- Total excess deaths in 2020  (WHO): 465,705 
- Total excess deaths in 2021  (WHO): 466,755 

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-35.png){width=960}
:::


#### South Africa 
- Total population: 59,308,690 
- Total official COVID deaths in 2020 (Our World in Data): 28,033 
- Total official COVID deaths in 2021  (Our World in Data): 63,028 
- Total excess deaths in 2020  (WHO): 54,401 
- Total excess deaths in 2021  (WHO):  

::: {.cell-output-display}
![](covidmort-who-sexage-results_files/figure-html/unnamed-chunk-6-36.png){width=960}
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

