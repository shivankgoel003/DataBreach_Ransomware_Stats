#### Preamble ####
# Purpose: Clean the cyberattack dataset for analysis on cyber breaches
# Author: Shivank Goel
# Date: 22 March 2024 
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(arrow)
library(dplyr)

#### Clean data ####

# Reading dataset
breach_data <- read_excel("data/raw_data/Dataset-cyberattacks-06102020.xlsx", sheet = 2)

# Simplifying names
breach_data <- clean_names(breach_data)


# Ensure 'attack_type' is treated as a character
breach_data$attack_type <- as.character(breach_data$attack_type)

# Replace both NA values and "NA" strings with "Unknown" in 'attack_type' column
breach_data <- breach_data %>%
  mutate(attack_type = ifelse(is.na(attack_type) | attack_type == "NA", "Unknown", attack_type))

# Replace both NA values and "NA" strings with "Unknown" in 'organisation_size' column
breach_data <- breach_data %>%
  mutate(organisation_size = ifelse(is.na(organisation_size) | organisation_size == "NA", "Unknown", organisation_size))
breach_data <- breach_data %>%
  filter(number_of_users_affected != "Missing")

# Convert 'number_of_users_affected' to numeric, handling non-numeric cases
breach_data$number_of_users_affected <- as.numeric(as.character(breach_data$number_of_users_affected))

# Remove unwanted columns
# Remove unwanted columns using dplyr::select to avoid any masking issues
breach_data <- dplyr::select(breach_data, -c(summary, effect_on_share_price, inappropriate_remote_access))


# Saving cleaned data
write.csv(breach_data, file = "data/analysis_data/breach_data.csv", row.names = FALSE)
write_parquet(breach_data, "data/analysis_data/breach_data.parquet")
