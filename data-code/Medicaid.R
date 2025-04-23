
# Meta --------------------------------------------------------------------

## Title:  Medicaid Expansion
## Author: Martinna Roldan
## Date Created: 04/18/2025
## Date Edited:  04/18/2025


# Preliminaries -----------------------------------------------------------

library(tidycensus)
library(dplyr)
library(readr)

kff.dat <- read_csv('data/input/raw_data.csv')

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.final$splitvar <- kff.final %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.final %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(State, expanded, date_adopted)

write_tsv(kff.final,'data/output/medicaid_expansion.txt')
