
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 5 Answers
## Author:        Martinna Roldan
## Date Created:  04/28/2025
## Date Edited:   04/28/2025
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               modelsummary, kableExtra, broom, cobalt, fixest)


# Read data and set workspace for knitr -------------------------------
final.data <- read_tsv('data/output/acs_medicaid.txt')

final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop) %>%
  filter(! State %in% c("Puerto Rico", "District of Columbia"))



# Create objects for markdown ---------------------------------------------


## Summary, Q1 - Share of direct purchase
direct.plot <- final.data %>%
  group_by(year) %>%
  summarize(mean = mean(perc_direct)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  geom_vline(xintercept = 2013.5, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Share of Direct Purchase Insurance Over Time",
    subtitle = "Red line = ACA Expansion (2014)",
    x = "Year",
    y = "Fraction with Direct Purchase"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

print(direct.plot)

## Summary, Q3 - Share of medicaid
medicaid.plot <- final.data %>%
  group_by(year) %>%
  summarize(mean = mean(perc_medicaid)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line(size = 1.2, color = "#009E73") +
  geom_point(size = 2, color = "#009E73") +
  geom_vline(xintercept = 2013.5, color = "red", linetype = "dashed", size = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Share of Medicaid Insurance Over Time",
    subtitle = "Red line = ACA Expansion (2014)",
    x = "Year",
    y = "Fraction with Medicaid"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold")
  )

print(medicaid.plot)
## Summary, Q4 - Share uninsured
ins.plot.dat <- final.data %>%
  filter(is.na(expand_year) | expand_year == 2014) %>%
  group_by(expand_ever, year) %>%
  summarize(mean = mean(perc_unins))

uninsurance.plot <- ggplot(ins.plot.dat, aes(x = year, y = mean, group = expand_ever, linetype = expand_ever)) +
  geom_line(size = 1.2, aes(color = expand_ever)) +
  geom_point(size = 2, aes(color = expand_ever)) +
  geom_vline(xintercept = 2013.5, color = "red", linetype = "dashed", size = 1) +
  scale_color_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#0072B2")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Share of Uninsured Over Time by Medicaid Expansion",
    subtitle = "Red line = ACA Expansion (2014)",
    x = "Year",
    y = "Fraction Uninsured",
    color = "Expansion Status",
    linetype = "Expansion Status"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(uninsurance.plot)

## ATE, Q1 - DD Table
dd.table <- final.data %>% 
  filter(is.na(expand_year) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%  
  group_by(expand_ever, year) %>%
  summarize(uninsured=mean(perc_unins))

dd.table <- pivot_wider(dd.table, names_from="year", names_prefix="year", values_from="uninsured") %>% 
  ungroup() %>%
  mutate(expand_ever=case_when(
    expand_ever==FALSE ~ 'Non-expansion',
    expand_ever==TRUE ~ 'Expansion')
  ) %>%
  rename(Group=expand_ever,
         Pre=year2012,
         Post=year2015)
print(dd.table)
## ATE, Q2/3 - DD regression estimates, 2014 expansion only (with and without state/year fixed effects)
reg.data <- final.data %>% mutate(post=(year>=2014),
                                  treat=post*expand_ever) %>%
  filter(is.na(expand_year) | expand_year==2014)

dd.est <- lm(perc_unins~post + expand_ever + treat, data=reg.data)
fe.est <- feols(perc_unins~treat | State + year, data=reg.data)
print(dd.est)
print(summary(dd.est))
print(fe.est)
print(summary(fe.est))

## ATE, Q4 - DD with time varying treatment
reg.data2 <- final.data %>% 
  mutate(treat=case_when(
    year>=expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    year<expand_year & !is.na(expand_year) ~ 0)
  )
fe.est2 <- feols(perc_unins~treat | State + year, data=reg.data2)
print(fe.est2)

## ATE, Q5 - Event study with constant treatment
mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.data)
print(mod.twfe)

# Create a tidy dataframe of estimates and standard errors
event.plot.data <- broom::tidy(mod.twfe) %>%
  filter(str_detect(term, "year::")) %>%
  mutate(
    year = as.numeric(str_remove(str_remove(term, "year::"), ":expand_ever")),
    estimate = estimate,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Plot the Event Study
event.study.plot <- ggplot(event.plot.data, aes(x = year, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_vline(xintercept = 2013, linetype = "dashed") + 
  geom_hline(yintercept = 0, color = "black") +
  theme_minimal(base_size = 14) +
  labs(
    title = NULL,
    x = "Time to treatment",
    y = "Estimate and 95% Conf. Int."
  )

print(event.study.plot)


## ATE, Q6 - Event study with time varying treatment
reg.data2 <- reg.data2 %>%
  mutate(time_to_treat=ifelse(expand_ever==TRUE, year-expand_year, -1),
         time_to_treat=ifelse(time_to_treat<=-4, -4, time_to_treat))

mod.twfe2 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.data2)
print(mod.twfe2)
# Tidy dataframe for plotting
event.plot.data2 <- broom::tidy(mod.twfe2) %>%
  filter(str_detect(term, "time_to_treat::")) %>%
  mutate(
    time_to_treat = as.numeric(str_remove(str_remove(term, "time_to_treat::"), ":expand_ever")),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Now make the plot
event.study.plot2 <- ggplot(event.plot.data2, aes(x = time_to_treat, y = estimate)) +
  geom_point(size = 1.5) +  # smaller dots
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +  # thinner error bars
  geom_vline(xintercept = -1, linetype = "dashed") +  # FIX: dashed line at -1
  geom_hline(yintercept = 0, color = "black") +
  theme_bw(base_size = 14) +   # use theme_bw like your professor
  labs(
    title = NULL,
    x = "Time to treatment",
    y = "Estimate and 95% Conf. Int."
  ) +
  scale_x_continuous(breaks = seq(-4, 5, by = 1))  # nicer x-axis ticks

print(event.study.plot2)
rm(list=c("final.data","ins.plot.dat"))
save.image("analysis/Hwk5_workspace.Rdata")


