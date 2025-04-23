# Meta --------------------------------------------------------------------

## Title:  American Community Survey Data
## Author: Martinna Roldan
## Date Created: 04/18/2025
## Date Edited:  04/18/2025

# Preliminaries -----------------------------------------------------------

library(tidycensus)
library(dplyr)
library(readr)

# Load your Census API key for the session
readRenviron("~/.Renviron")

# Set the variables you want to pull
variables <- c(
  all_18to34 = "B27010_018",
  employer_18to34 = "B27010_020",
  direct_18to34 = "B27010_021",
  medicare_18to34 = "B27010_022",
  medicaid_18to34 = "B27010_023",
  tricare_18to34 = "B27010_024",
  va_18to34 = "B27010_025",
  none_18to34 = "B27010_033",
  all_35to64 = "B27010_034",
  employer_35to64 = "B27010_036",
  direct_35to64 = "B27010_037",
  medicare_35to64 = "B27010_038",
  medicaid_35to64 = "B27010_039",
  tricare_35to64 = "B27010_040",
  va_35to64 = "B27010_041",
  none_35to64 = "B27010_050"
)

# Retrieve ACS data -------------------------------------------------------

all_years_data <- list()

for (t in 2013:2019) {
  temp <- get_acs(
    geography = "state",
    variables = variables,
    year = t,
    survey = "acs5",
    output = "wide"
  ) %>%
    rename(State = NAME) %>%
    rename_with(~ gsub("E$", "", .x)) %>%  # <-- remove "E"
    mutate(year = t)
  
  all_years_data[[as.character(t)]] <- temp
}


# Combine all years into one data frame
final.insurance <- bind_rows(all_years_data)

# Tidy data ---------------------------------------------------------------

final.insurance <- final.insurance %>%
  mutate(
    adult_pop = all_18to34 + all_35to64,
    ins_employer = employer_18to34 + employer_35to64,
    ins_direct = direct_18to34 + direct_35to64,
    ins_medicare = medicare_18to34 + medicare_35to64,
    ins_medicaid = medicaid_18to34 + medicaid_35to64,
    uninsured = none_18to34 + none_35to64
  ) %>%
  select(State, year, adult_pop, ins_employer, ins_direct, 
         ins_medicare, ins_medicaid, uninsured)

# Write output ------------------------------------------------------------

# Make sure the folder "data/output" exists first
if (!dir.exists("data/output")) {
  dir.create("data/output", recursive = TRUE)
}

write_tsv(final.insurance, 'data/output/acs_insurance.txt')
