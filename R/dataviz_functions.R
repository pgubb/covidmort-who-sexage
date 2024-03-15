


#############################################################
# Theme for charts -------------------------------------------------------------
#############################################################

theme_custom <- function(tisize = 11.5, txtsize = 10, axsize = 9, scale_f = 1, ...) {
  theme_gray() +
    theme(
      plot.margin = margin(3, 1.5, 1.5, 1.5, "pt"),
      plot.subtitle = element_text(size = txtsize*scale_f),
      plot.title = element_text(size = tisize*scale_f, hjust = 0, face = 'bold'),
      axis.text.x = element_text(hjust = 0.5),
      axis.text.y = element_text(hjust = 1),
      axis.title.y = element_text(color="black", size = txtsize*scale_f, angle = 90, vjust = 1, hjust = 1),
      axis.title.x = element_text(color="black", hjust = 0.5, size=txtsize*scale_f),
      axis.text = element_text(size=axsize*scale_f, color = "black"),
      #panel.grid = element_line(color = "#F5E3E0", size = 0.25),
      plot.caption=element_text(hjust=0, color="grey40", size=txtsize*scale_f),
      panel.background = element_rect(fill = "#ffffff"),
      rect = element_rect(fill = "#ffffff", colour = "#ffffff", size = 0.5, linetype = 1),
      axis.line.x = element_line(color = "black", size=0.5),
      axis.line.y = element_line(color = "black", size=0.5),
      strip.background = element_rect(fill = "#ffffff"),
      strip.text = element_text(size = txtsize*scale_f, face = 'bold'),
      strip.text.y = element_text(angle = 90),
      legend.title = element_text(size = txtsize*scale_f),
      legend.text = element_text(size = txtsize*scale_f),
      legend.key.height = unit(1, 'lines'),
      legend.key.width = unit(1, 'lines'),
      legend.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
      legend.key = element_rect(colour = "transparent", fill = "white"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      ...
    )
}

#############################################################################
# FUNCTION THAT PRODUCES A CHART WITH ALL DATA FOR EACH COUNTRY ------------
#############################################################################

plot_obs_and_preds <- function(predictions, observations, iso) {

  # This function produces a chart that shows the predictions and observations for mortality rates, relative risk and absolute differences in mortality

  country_name <- unique(pull(predictions %>% filter(iso3c == iso), Country))

  colors <-  viridis(5, option = "D")
  colors <-  colors[c(1,3)]
  #bgcolor <- "#D7D0C8"
  bgcolor <- "#F4EBE8"

  subtitle <- glue::glue(
    "
    Absolute and relative risk of mortality for <span style = 'color:{colors[1]};'>**males**</span> and <span style = 'color:{colors[2]};'>**females**</span> by type of death and age group
    "
  )

  # CONTRAST 1
  ggplot(predictions %>% filter(iso3c == iso),
         aes(
           x = Age_Lower,
           color = Sex_name,
           group = Sex_name,
           alpha = type
         )) +

    facet_grid(rows = vars(metric_lab), cols = vars(source_year), switch = "y", scales = "free_y") +

    # Country fit
    geom_line(aes(y = logp50), size = 2, show.legend = TRUE) +
    #geom_point(size = 1, alpha = 0.6) +

    # Uncertainty bound: smooth based on 95th/5th percentile
    geom_ribbon(
      aes(
        ymin=logp5,
        ymax=logp95,
        fill = Sex_name
      ),
      size = 0.25,
      alpha=0.25, show.legend = FALSE) +

    # Country data
    #geom_line(data = observations %>% filter(iso3c == iso), aes(y = value, color = sex)) +
    geom_point(data = observations %>% filter(iso3c == iso), aes(y = logval, color = Sex_name), shape=21,fill ="white", size = 2, stroke = 1, show.legend = TRUE) +

    # Guides
    guides(
      color = "none",
      fill = "none"
    ) +

    # Scales

    scale_color_manual(values = c("Females" = colors[2], "Males" = colors[1], "Not applicable" = "grey60")) +
    scale_fill_manual(values = c("Females" = colors[2], "Males" = colors[1], "Not applicable" = "grey60")) +
    scale_alpha_manual(name = NULL,
                       values = c(1, 1),
                       breaks = c("Observation", "Prediction"),
                       guide = guide_legend(override.aes = list(linetype = c(0, 1),
                                                                shape = c(21, NA),
                                                                fill = "white",
                                                                color = "black")) ) +
    # Plot labels
    labs(
      title = country_name,
      subtitle = subtitle,
      source = "Author's calculations with WHO excess mortality dataset",
      y = NULL,
      x = "Age (Lower bound)"
    ) +

    # THemes
    theme_custom(scale_f = 1.2) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    theme(plot.background = element_rect(fill = bgcolor),
          panel.background = element_rect(fill = bgcolor),
          strip.background = element_rect(fill = bgcolor),
          legend.background = element_rect(fill = bgcolor, color = bgcolor),
          plot.subtitle = element_markdown(),
          strip.text.y = element_text(angle = 0))


  # chart_width = 12
  # chart_height = 9
  # file <- paste0("Results/Figures/obs_and_preds_", iso, ".png")
  # ggsave(file, width = chart_width, height = chart_height, dpi = 300)

}

##################################################################################
# FUNCTION THAT PRODUCES WITH ACTUAL SEX-RATIOs by COUNTRY (Facetted) ------------
##################################################################################

plot_obs <- function(observations, slct_source, slct_metric = "Relative risk (Males/Females)", title = NULL) {

  colors <-  viridis(5, option = "D")
  colors <-  colors[c(1,3)]
  #bgcolor <- "#D7D0C8"
  bgcolor <- "#F4EBE8"

  data_to_plot <- observations %>%
    filter(metric == slct_metric) %>%
    filter(source %in% slct_source) %>%
    filter(Year == 2020)


  if (!is.null(title)) {
    title = title
  } else{
    title = paste0(unique(data_to_plot$metric), " in mortality")
  }

  # CONTRAST 1
  ggplot(data_to_plot,
         aes(
           x = Age_Lower,
           y = value,
           color = source_year,
           group = source_year
         )) +

    facet_wrap(~Country, ncol = 6, scales = "free_y") +

    # Plot elements
    geom_hline(yintercept = 1, size = 1.75, color = "white") +

    geom_line() +
    geom_point(shape=21,fill ="white", size = 2, stroke = 1, show.legend = TRUE) +

    guides(
      color = guide_legend(title = "")
    ) +

    # Scales
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    scale_color_manual(values = c("All-cause deaths (actual): 2020" = "red", "All-cause deaths (expected): 2020" = "grey50", "Excess deaths: 2020" = colors[1])) +

    # Plot labels
    labs(
      title = title,
      subtitle = "",
      caption = "Note: Author's calculation from the WHO Global Excess Deaths dataset",
      y = NULL,
      x = "Age (Lower bound)"
    ) +

    # THemes
    theme_custom(scale_f = 1.2) +
    theme(legend.position = "top", legend.direction = "horizontal") +
    theme(plot.background = element_rect(fill = bgcolor),
          panel.background = element_rect(fill = bgcolor),
          strip.background = element_rect(fill = bgcolor),
          legend.background = element_rect(fill = bgcolor, color = bgcolor),
          plot.subtitle = element_markdown(),
          strip.text.y = element_text(angle = 0),
          strip.text.x = element_text(face = "plain", hjust = 0))


}

##################################################################################
# FUNCTION THAT PRODUCES A FIGURE OF RELATIONSHIP WITH GDP PER CAPITA ------------
##################################################################################

plot_data_v_income <- function(data, slct_metric, income_data, group_data = NULL, caption, alpha = 0.75, ylims = c(0, 10)) {

# alpha represents the Parameter for loess fit

# Data type (Prediction v Observation)
  datatype <- unique(data$type)

# Chart aesthetics and labels ----------------

  colors <-  viridis(5, option = "D")
  bgcolor <- "#F4EBE8"
  subtitle <- glue::glue(
    "
    Age-groups:
    <span style = 'color:{colors[1]};'>45-54 years</span>,
    <span style = 'color:{colors[2]};'>55-65</span>,
    <span style = 'color:{colors[3]};'>65-74</span>,
    <span style = 'color:{colors[4]};'>75-84</span>,
    <span style = 'color:{colors[5]};'>85+</span>
    "
  )

# Chart axis/scale parameters -----------

if (slct_metric == "Mortality rate (per 100,000) [log-scale]") {
    file_suffix <- "mr"
    trans <- "log"
    breaks <- c(0.1, 1, 10, 100, 1000, 6400)
    ylims <- NULL
    chart_height = 12
  } else if (slct_metric == "Relative risk (Males/Females)") {
    file_suffix <- "rr"
    trans <- "identity"
    ylims <- ylims
    breaks <- seq(ylims[1], ylims[2], by = 2)
    chart_height = 7.5
  } else {
    file_suffix <- "ar"
    trans <-"identity"
    breaks <- c(-2000, -1000, 0, 1000, 2000, 3000, 4000)
    ylims <- NULL
    chart_height = 7.5
  }

  # Filtering

  data_to_plot <- data %>%
    filter(metric == slct_metric) %>%
    mutate(Age_Lower = as.factor(Age_Lower))

  # Joining income per capita to mortality data ---------

  data_to_plot <- data_to_plot %>%
    left_join(income_data, by = c("iso3c"))

# Joining group data to mortality data ---------

  if (!is.null(group_data)) {
    data_to_plot <- data_to_plot %>%
      left_join(group_data, by = c("iso3c")) %>%
      filter(reported_gender_2020 == 1 & reported_gender_2021 == 1) %>%
      filter()
    }

  # For prediction data creating the loess that tracks expected value over GNI per capita --------

if (datatype == "Prediction") {

    data_to_plot %>%
    group_by(source, Year, Sex_name, Age_Lower) %>%
    mutate(
      loess_fit_hi = predict(loess(p75 ~ gni_pcap_ppp, span = alpha)),
      loess_fit_mid = predict(loess(p50 ~ gni_pcap_ppp, span = alpha)),
      loess_fit_lo = predict(loess(p25 ~ gni_pcap_ppp, span = alpha))
    ) -> data_to_plot

  if (slct_metric == "Mortality rate (per 100,000) [log-scale]") {
     data_to_plot %>%
      # Adjusting if loess fit is negative
      mutate(
        loess_fit_hi = ifelse(loess_fit_hi <= 0, 1e-2, loess_fit_hi),
        loess_fit_mid = ifelse(loess_fit_mid <= 0, 1e-2, loess_fit_mid),
        loess_fit_lo = ifelse(loess_fit_lo <= 0, 1e-2, loess_fit_lo)
      )  -> data_to_plot

  }

}

  # Computing number of countries by source & year

  data_to_plot <- data_to_plot %>%
    group_by(source_year) %>%
    mutate(
      Ncountries = length(unique(iso3c)),
      facet_label = paste(source_year, "\n", "N countries =", Ncountries)
    )

  # Chart elements ----------------

    p <-
      ggplot(
      data = data_to_plot,
      aes(
        x = gni_pcap_ppp,
        color = Age_Lower
      )
    )

  if (slct_metric == "Mortality rate (per 100,000) [log-scale]") {

    p <- p +
              facet_grid(rows = vars(Sex_name), cols = vars(source_year), switch = "y") +
              geom_hline(yintercept = 0, color = "black", size = 1)

  } else {

    p <- p +
            facet_wrap(~facet_label, nrow = 1) +
            geom_hline(yintercept = 1, color = "white", size = 2)
  }

  if (datatype == "Prediction") {

  p <- p +

  #Uncertainty bound: smooth based on 95th/5th percentile
  geom_ribbon(
    aes(
      ymin=loess_fit_lo,
      ymax=loess_fit_hi,
      fill = Age_Lower
    ),
    size = 0.25,
    alpha=0.25, show.legend = FALSE) +

  # Central tendency
  geom_line(aes(y = loess_fit_mid), size = 1.5) +
  geom_point(aes(y = p50), shape = 21, fill = "white", size = 2, stroke = 1)

  } else {

      if (slct_metric %in% c("Relative risk (Males/Females)", "Mortality rate (per 100,000) [log-scale]")) {
        p <- p +
        geom_smooth(aes(y = value_adj, group = Age_Lower, fill = Age_Lower), se = TRUE, method = "loess", span = alpha) +
        geom_point(aes(y = value_adj), shape = 21, fill = "white", size = 2, stroke = 1)
      } else {
        p <- p +
        geom_smooth(aes(y = value, group = Age_Lower, fill = Age_Lower), se = TRUE, method = "loess", span = alpha) +
        geom_point(aes(y = value), shape = 21, fill = "white", size = 2, stroke = 1)
        }
    }

  # Scales and labels

  p +

  scale_color_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +

  scale_y_continuous(trans = trans, breaks = breaks) +
  scale_x_continuous(labels = scales::label_dollar()) +

  guides(color = guide_legend(title = "Age group (lower bound):"),
         fill = "none") +

  labs(
    #title = paste0(slct_metric, " (", datatype, ")"),
    title = NULL,
    subtitle = subtitle,
    y = NULL,
    x = "GNI per capita (PPP)",
    caption = caption
  ) +

  coord_cartesian(ylim = ylims) +

  theme_custom(scale_f = 1.5) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  theme(plot.background = element_rect(fill = bgcolor),
        panel.background = element_rect(fill = bgcolor),
        strip.background = element_rect(fill = bgcolor),
        legend.background = element_rect(fill = bgcolor, color = bgcolor),
        plot.subtitle = element_markdown())

# chart_width = 12
# file <- paste0("Results/Figures/results_v_income_", file_suffix, ".png")
# ggsave(file, width = chart_width, height = chart_height, dpi = 300)

}

##################################################################################
# FUNCTION THAT PRODUCES A FIGURE OF MORTALITY RATE RATIO VS INCOME TERCILE ------------
##################################################################################

plot_data_v_tercile <- function(data, slct_metric) {

  #
  data_to_plot <- data %>% filter(metric == slct_metric) %>% filter(source != "All-cause deaths (actual)")

  # caption

  caption <- "Notes: Results are based on the country-specific Poisson model described in the text applied to the WHO Global Excess dataset.  It shows the median and interquartile ranges (uncertainty bars) of predicted mortality sex ratio for each age group by country income (in three groups) as defined in Table 1. "

  # Chart aesthetics and labels ----------------

  colors <-  viridis(5, option = "D")
  bgcolor <- "#F4EBE8"
  subtitle <- glue::glue(
    "
    Age-groups:
    <span style = 'color:{colors[1]};'>45-54 years</span>,
    <span style = 'color:{colors[2]};'>55-65</span>,
    <span style = 'color:{colors[3]};'>65-74</span>,
    <span style = 'color:{colors[4]};'>75-84</span>,
    <span style = 'color:{colors[5]};'>85+</span>
    "
  )

ggplot(
  data = data_to_plot,
  aes(x = gnipcap_quintiles, y = p50, color = Age_Lower, fill = Age_Lower, group = Age_Lower)
) +
  #facet_grid(cols = vars(source_year), rows = vars(Age_Lower), switch = "y") +
  facet_wrap(~source_year, nrow = 1) +
  geom_crossbar(aes(ymin = p25, ymax = p75), width = 0.2, position=position_dodge(width=0.5), alpha = 0.5) +
  geom_line(position=position_dodge(width=0.5)) +
  scale_color_viridis(option = "D") +
  scale_fill_viridis(option = "D") +
  scale_x_continuous(breaks = c(1, 2, 3)) +
  labs(
       subtitle = subtitle,
       y = "Mortality sex-ratio (M/F)",
       x = "GNI per capita tercile",
       caption = str_wrap(caption, 115)
       ) +
  theme_custom(scale_f = 1.5) +
  theme(legend.position = "none", legend.direction = "horizontal") +
  theme(plot.background = element_rect(fill = bgcolor),
        panel.background = element_rect(fill = bgcolor),
        strip.background = element_rect(fill = bgcolor),
        legend.background = element_rect(fill = bgcolor, color = bgcolor),
        plot.subtitle = element_markdown())

}

################################################################
# FUNCTION THAT VISUALIZES REGRESSION COEFFICIENTS --------
################################################################

visualize_coeffs <- function(model_results, models = NULL, coefs = NULL) {

  model_results %>%
    filter(source  != "All-cause deaths (actual)") -> model_results

  model_results %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles = cut(gni_pcap_ppp, breaks = quantile(gni_pcap_ppp, probs = seq(0, 1, 1/3)), label = FALSE, ordered_result = TRUE, include.lowest = TRUE),
      gnipcap_terciles_lab = paste("Tercile", gnipcap_terciles)
    ) -> model_results

  caption = "Note: Columns 1-4 are regression coefficients based on the country-specific Poisson model described in the text applied to the WHO Global Excess Deaths dataset.
  Column 1 is the predicted female mortality per 100,000 at age 65.
  Column 2 is the predicted ratio of male-to-female mortality at age 65.
  Columns 3 & 4 is the increase in mortality risk for women and men respectively associated with an additional 10 years in age.
  In 2020, for all age groups at or above 55, excess death estimates for males in Thailand are negative, since these values were bottom coded to 0 the sex-ratio at age 65 is 0 and the association of excess deaths with age for males is also 0.
  Countries are sorted by GNI per capita (PPP)"

  if(!is.null(coefs)) {
    model_results %>% filter(term %in% coefs) -> model_results
  }

  if(!is.null(models)) {
    model_results %>% filter(model_label %in% models) -> model_results
  }

  # Computing the population weighted average coefficient

  model_results %>%
    group_by(source, Year, model_no, model_label, term_short, term_label2, term_label3) %>%
    summarize(
      estimate = weighted.mean(estimate,  w=NxTOT, na.rm = TRUE),
      estimate_interp = weighted.mean(estimate_interp,  w=NxTOT, na.rm = TRUE),
      estimate_interp2 = weighted.mean(estimate_interp2,  w=NxTOT, na.rm = TRUE),
      N = 1,
      Nobs = sum(N)
    ) %>%
    mutate(
      Country = "Average*",
      gni_pcap_ppp = -1e6,
      gnipcap_terciles_lab = ""
    ) %>%
    ungroup() -> average

  data_to_plot <- bind_rows(model_results)

  # By country & income group (Tile chart)
  data_to_plot <- data_to_plot %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles_lab = factor(gnipcap_terciles_lab, levels = c("", "Tercile 1", "Tercile 2", "Tercile 3"), ordered = TRUE),
      fillvalue = estimate_interp2,
      fillvalue = ifelse(estimate_interp2 >= 7, 7, fillvalue),
      fillvalue = ifelse(term_short == "a1", NA, fillvalue),
      maxfill = max(fillvalue, na.rm = TRUE),
      minfill = min(fillvalue, na.rm = TRUE),
      fillvalue = (fillvalue - minfill)/ (maxfill - minfill),
      #fillcolor = ifelse(estimate_interp < 1, "red", "blue"),
      #fillcolor = ifelse(exp(estimate) > 300, NA, estimate_interp),
      #fillcolor = ifelse(round(exp(estimate), 1) == 0, NA, estimate_interp),
      #fillcolor = ifelse(term_short == "a1", NA, estimate_interp),
      facet_label = paste0(source, ":", Year),
      value_label = ifelse(term_short == "a1", prettyNum(round(estimate_interp2, 1), big.mark = ","), round(estimate_interp2, 2)),
      group = paste(source, Year, iso3c, sep = "_"),
      drop = ifelse(source == "All-cause deaths (expected)" & Year == 2021, 1, 0),
      income = ifelse(Country == "Average*", "", income),
      income = factor(income, levels = c("", "Lower middle income", "Upper middle income", "High income"), ordered = TRUE)
    ) %>%
    filter(drop == 0)

  #subtitle <- str_wrap(unique(data_to_plot$term_label2), 180)

  ggplot(data = data_to_plot,
         aes(y = fct_reorder(Country, gni_pcap_ppp, .desc = TRUE),
             x = term_label3,
             fill = fillvalue)) +
    facet_grid(cols = vars(str_wrap(facet_label, 20)), rows = vars(gnipcap_terciles_lab), switch = "y", scales = "free_y", space = "free_y") +
    geom_tile(alpha = 0.7) +
    geom_text(aes(label = value_label), color = "black", size = 3.75) +
    scale_x_discrete(position = "top") +
    scale_color_identity() +
    #scale_fill_identity() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey70") +
    labs(
      title =  NULL,
      #subtitle = subtitle,
      y = NULL,
      x = NULL,
      caption = str_wrap(caption, 150)) +
    theme_custom(scale_f = 1.15, legend.position = "none") +
    theme(axis.text.y = element_text(size = 11))


}


visualize_coeffs_bytercile <- function(model_results, models = NULL, coefs = NULL) {

  model_results %>%
    filter(source  != "All-cause deaths (actual)") -> model_results

  model_results %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles = cut(gni_pcap_ppp, breaks = quantile(gni_pcap_ppp, probs = seq(0, 1, 1/3)), label = FALSE, ordered_result = TRUE, include.lowest = TRUE),
      gnipcap_terciles_lab = paste("Tercile", gnipcap_terciles)
    ) -> model_results

  caption = "Note: Columns 1-4 are regression coefficients based on the country-specific Poisson model described in the text applied to the WHO Global Excess Deaths dataset.
  Column 1 is the predicted female mortality per 100,000 at age 65.
  Column 2 is the predicted ratio of male-to-female mortality at age 65.
  Columns 3 & 4 is the increase in mortality risk for women and men respectively associated with an additional 10 years in age.
  This table shows the population weighted averages of the country specific coefficients shown in Table 2 overall and by income terciles."

  if(!is.null(coefs)) {
    model_results %>% filter(term %in% coefs) -> model_results
  }

  if(!is.null(models)) {
    model_results %>% filter(model_label %in% models) -> model_results
  }

  # Computing the population weighted average coefficient

  model_results %>%
    group_by(source, Year, model_no, model_label, term_short, term_label2, term_label3) %>%
    summarize(
      estimate = weighted.mean(estimate,  w=NxTOT, na.rm = TRUE),
      estimate_interp = weighted.mean(estimate_interp,  w=NxTOT, na.rm = TRUE),
      estimate_interp2 = weighted.mean(estimate_interp2,  w=NxTOT, na.rm = TRUE),
      N = 1,
      Nobs = sum(N)
    ) %>%
    mutate(
      Country = "Average (all countries)",
      gni_pcap_ppp = -1e6,
      gnipcap_terciles_lab = ""
    ) %>%
    ungroup() -> average

  model_results %>%
    group_by(source, gnipcap_terciles_lab, Year, model_no, model_label, term_short, term_label2, term_label3) %>%
    summarize(
      estimate = weighted.mean(estimate,  w=NxTOT, na.rm = TRUE),
      estimate_interp = weighted.mean(estimate_interp,  w=NxTOT, na.rm = TRUE),
      estimate_interp2 = weighted.mean(estimate_interp2,  w=NxTOT, na.rm = TRUE),
      N = 1,
      Nobs = sum(N)
    ) %>%
    mutate(
      Country = paste(gnipcap_terciles_lab, "average"),
      gni_pcap_ppp = -1e6,
      gnipcap_terciles_lab = ""
    ) %>%
    ungroup() -> average_by_tercile

  data_to_plot <- bind_rows(average, average_by_tercile)

  # By country & income group (Tile chart)
  data_to_plot <- data_to_plot %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles_lab = factor(gnipcap_terciles_lab, levels = c("", "Tercile 1", "Tercile 2", "Tercile 3"), ordered = TRUE),
      fillvalue = estimate_interp2,
      fillvalue = ifelse(estimate_interp2 >= 7, 7, fillvalue),
      fillvalue = ifelse(term_short == "a1", NA, fillvalue),
      maxfill = max(fillvalue, na.rm = TRUE),
      minfill = min(fillvalue, na.rm = TRUE),
      fillvalue = (fillvalue - minfill)/ (maxfill - minfill),
      #fillcolor = ifelse(estimate_interp < 1, "red", "blue"),
      #fillcolor = ifelse(exp(estimate) > 300, NA, estimate_interp),
      #fillcolor = ifelse(round(exp(estimate), 1) == 0, NA, estimate_interp),
      #fillcolor = ifelse(term_short == "a1", NA, estimate_interp),
      facet_label = paste0(source, ":", Year),
      value_label = ifelse(term_short == "a1", prettyNum(round(estimate_interp2, 1), big.mark = ","), round(estimate_interp2, 2)),
      drop = ifelse(source == "All-cause deaths (expected)" & Year == 2021, 1, 0),
      income = ifelse(Country == "Average*", "", income),
      income = factor(income, levels = c("", "Lower middle income", "Upper middle income", "High income"), ordered = TRUE)
    ) %>%
    filter(drop == 0)

  #subtitle <- str_wrap(unique(data_to_plot$term_label2), 180)

  ggplot(data = data_to_plot,
         aes(y = fct_rev(Country),
             x = term_label3,
             fill = fillvalue)) +
    facet_grid(cols = vars(str_wrap(facet_label, 20)), switch = "y", scales = "free_y", space = "free_y") +
    geom_tile(alpha = 0.7) +
    geom_hline(yintercept = 3.5, color = "grey50", size = 0.5) +
    geom_text(aes(label = value_label), color = "black", size = 3.75) +
    scale_x_discrete(position = "top") +
    scale_color_identity() +
    #scale_fill_identity() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey70") +
    labs(
      title =  NULL,
      #subtitle = subtitle,
      y = NULL,
      x = NULL,
      caption = str_wrap(caption, 150)) +
    theme_custom(scale_f = 1.15, legend.position = "none") +
    theme(axis.text.y = element_text(size = 11))


}

combine_tables <- function(model_results, models = NULL, coefs = NULL) {
  # This function creates a table that combines Tables 2 and 3 into a single, large table. 
  
  model_results %>%
    filter(source  != "All-cause deaths (actual)") -> model_results
  
  model_results %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles = cut(gni_pcap_ppp, breaks = quantile(gni_pcap_ppp, probs = seq(0, 1, 1/3)), label = FALSE, ordered_result = TRUE, include.lowest = TRUE),
      gnipcap_terciles_lab = paste("Tercile", gnipcap_terciles)
    ) -> model_results
  
  caption = "Note: Columns 1-4 are regression coefficients based on the country-specific Poisson model described in the text applied to the WHO Global Excess Deaths dataset.
  Column 1 is the predicted female mortality per 100,000 at age 65.
  Column 2 is the predicted ratio of male-to-female mortality at age 65.
  Columns 3 & 4 is the increase in mortality risk for women and men respectively associated with an additional 10 years in age.
  The first four rows in this table show the population weighted averages of the country specific coefficients overall and by income terciles.
  In 2020, for all age groups at or above 55, excess death estimates for males in Thailand are negative, since these values were bottom coded to 0 the sex-ratio at age 65 is 0 and the association of excess deaths with age for males is also 0.
  Countries are sorted by GNI per capita (PPP)"
  
  if(!is.null(coefs)) {
    model_results %>% filter(term %in% coefs) -> model_results
  }
  
  if(!is.null(models)) {
    model_results %>% filter(model_label %in% models) -> model_results
  }
  
  # Computing the population weighted average coefficient
  
  model_results %>%
    group_by(source, Year, model_no, model_label, term_short, term_label2, term_label3) %>%
    summarize(
      estimate = weighted.mean(estimate,  w=NxTOT, na.rm = TRUE),
      estimate_interp = weighted.mean(estimate_interp,  w=NxTOT, na.rm = TRUE),
      estimate_interp2 = weighted.mean(estimate_interp2,  w=NxTOT, na.rm = TRUE),
      N = 1,
      Nobs = sum(N)
    ) %>%
    mutate(
      Country = "Average (all countries)",
      gni_pcap_ppp = -1e6,
      gnipcap_terciles_lab = ""
    ) %>%
    ungroup() -> average
  
  model_results %>%
    group_by(source, gnipcap_terciles_lab, Year, model_no, model_label, term_short, term_label2, term_label3) %>%
    summarize(
      estimate = weighted.mean(estimate,  w=NxTOT, na.rm = TRUE),
      estimate_interp = weighted.mean(estimate_interp,  w=NxTOT, na.rm = TRUE),
      estimate_interp2 = weighted.mean(estimate_interp2,  w=NxTOT, na.rm = TRUE),
      N = 1,
      Nobs = sum(N)
    ) %>%
    mutate(
      Country = paste(gnipcap_terciles_lab, "average"),
      gni_pcap_ppp = ifelse(gnipcap_terciles_lab == "Tercile 1", 1, ifelse(gnipcap_terciles_lab == "Tercile 2", 2, 3)),
      gnipcap_terciles_lab = ""
    ) %>%
    ungroup() -> average_by_tercile
  
  data_to_plot <- bind_rows(model_results, average, average_by_tercile)
  
  # By country & income group (Tile chart)
  data_to_plot <- data_to_plot %>%
    ungroup() %>%
    mutate(
      gnipcap_terciles_lab = factor(gnipcap_terciles_lab, levels = c("", "Tercile 1", "Tercile 2", "Tercile 3"), ordered = TRUE),
      fillvalue = estimate_interp2,
      fillvalue = ifelse(estimate_interp2 >= 7, 7, fillvalue),
      fillvalue = ifelse(term_short == "a1", NA, fillvalue),
      maxfill = max(fillvalue, na.rm = TRUE),
      minfill = min(fillvalue, na.rm = TRUE),
      fillvalue = (fillvalue - minfill)/ (maxfill - minfill),
      #fillcolor = ifelse(estimate_interp < 1, "red", "blue"),
      #fillcolor = ifelse(exp(estimate) > 300, NA, estimate_interp),
      #fillcolor = ifelse(round(exp(estimate), 1) == 0, NA, estimate_interp),
      #fillcolor = ifelse(term_short == "a1", NA, estimate_interp),
      facet_label = paste0(source, ":", Year),
      value_label = ifelse(term_short == "a1", prettyNum(round(estimate_interp2, 1), big.mark = ","), round(estimate_interp2, 2)),
      drop = ifelse(source == "All-cause deaths (expected)" & Year == 2021, 1, 0),
      income = ifelse(Country == "Average*", "", income),
      income = factor(income, levels = c("", "Lower middle income", "Upper middle income", "High income"), ordered = TRUE)
    ) %>%
    filter(drop == 0)
  
  #subtitle <- str_wrap(unique(data_to_plot$term_label2), 180)
  
  ggplot(data = data_to_plot,
         aes(y = fct_reorder(Country, gni_pcap_ppp, .desc = TRUE),
             x = term_label3,
             fill = fillvalue)) +
    facet_grid(cols = vars(str_wrap(facet_label, 20)), rows = vars(gnipcap_terciles_lab), switch = "y", scales = "free_y", space = "free_y") +
    geom_tile(alpha = 0.7) +
    #geom_hline(yintercept = 3.5, color = "grey50", size = 0.5) +
    geom_text(aes(label = value_label), color = "black", size = 3.75) +
    scale_x_discrete(position = "top") +
    scale_color_identity() +
    #scale_fill_identity() +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey70") +
    labs(
      title =  NULL,
      #subtitle = subtitle,
      y = NULL,
      x = NULL,
      caption = str_wrap(caption, 150)) +
    theme_custom(scale_f = 1.15, legend.position = "none") +
    theme(axis.text.y = element_text(size = 11))
  
  
}

#combine_tables(model_results, models = "3. Adjusted model (Deaths ~ a + b*male + c*age + d*male*age)")
#ggsave("public_ignore/main figures/table23_combined.png", width = 12, height = 18, dpi = 300)

