#### Preamble ####
# Purpose: Clean the cyberattack dataset for analysis on breaches from 2010 to 2020
# Author: Shivank Goel
# Date: 22 March 2024 
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
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

breach_data$number_of_users_affected <- as.numeric(as.character(breach_data$number_of_users_affected))


# Saving cleaned data
write.csv(breach_data, file = "data/analysis_data/breach_data.csv", row.names = FALSE)

