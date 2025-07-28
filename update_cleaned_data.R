# Load required libraries
library(dplyr)
library(readr)

# Read the original data
data <- read.csv("Most-Recent-Cohorts-Institution.csv")

# Create cleaned_data with new variables
cleaned_data <- data %>%
  # Calculate ACT Median
  mutate(ACT_MEDIAN = ACTWRMID + ACTMTMID) %>%
  mutate(GRAD_DEBT_MDN = na_if(GRAD_DEBT_MDN, "PrivacySuppressed")) %>%
  mutate(GRAD_DEBT_MDN = as.numeric(GRAD_DEBT_MDN)) %>%
  
  # Select only relevant columns
  dplyr::select(
    INSTNM, INSTURL, CONTROL, SAT_AVG, ADM_RATE, LOCALE, ACT_MEDIAN, GRAD_DEBT_MDN, UGDS, PCTPELL,
    UGDS_WHITE, UGDS_BLACK, UGDS_HISP, UGDS_ASIAN, UGDS_AIAN, UGDS_NHPI, UGDS_2MOR, UGDS_NRA, UGDS_UNKN,
    C200_4, COSTT4_A, CITY, STABBR
  ) %>%
  
  # Drop rows that have NA values for all SAT_AVG, ADM_RATE, and LOCALE
  filter(!(is.na(SAT_AVG) & is.na(ADM_RATE) & is.na(LOCALE))) %>%
  
  # Drop rows that have NA values for all UGDS_'RACE'
  filter(!(is.na(UGDS_WHITE) & is.na(UGDS_BLACK) & is.na(UGDS_HISP) & is.na(UGDS_ASIAN) & is.na(UGDS_AIAN) &
             is.na(UGDS_NHPI) & is.na(UGDS_2MOR) & is.na(UGDS_NRA) & is.na(UGDS_UNKN))) %>%
  
  # Binary variable for colleges that are public and is in city or suburb
  mutate(
    IS_PUBLIC = case_when(
      CONTROL == 1 ~ 1,
      CONTROL %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    IS_CITY = case_when(
      LOCALE >= 11 & LOCALE <= 23 ~ 1,
      LOCALE >= 31 & LOCALE <= 43 ~ 0,
      TRUE ~ NA_real_
    )) %>%
  
  # Remove column that is no longer needed
  select(!c("CONTROL", "LOCALE"))

# Write to csv file
write_csv(cleaned_data, "cleaned_data.csv")

print("cleaned_data.csv updated successfully!") 