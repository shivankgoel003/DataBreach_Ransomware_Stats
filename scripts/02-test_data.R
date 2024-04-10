#### Preamble ####
# Purpose: Data Validation and Testing for Cyberattack Data Analysis
# Authors: Shivank Goel
# Date: 10th April 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, testthat

#### Workspace setup ####
library(tidyverse)
library(testthat)
library(here)

# Load the datasets
breach_data <- read.csv(here("data/analysis_data/breach_data.csv"))

#### Test data ####

# Test the structure and type of the breach data
test_that("Breach data columns are of the correct type", {
  expect_type(breach_data$year, "integer")
  expect_type(breach_data$organisation, "character")
  expect_type(breach_data$number_of_users_affected, "double")
  expect_type(breach_data$sector, "character")
  expect_type(breach_data$country, "character")
  # ... Add more checks as needed
})

# Test that the years are within the expected range
test_that("Year values are within the expected range", {
  expect_true(all(breach_data$year >= 2004 & breach_data$year <= 2019))
})

# Test for critical industry indicator
test_that("Critical industry values are valid", {
  valid_critical_indicators <- c("Yes", "No")
  expect_true(all(breach_data$critical_industry %in% valid_critical_indicators))
})

# Test the structure and type of the organisation size
test_that("Organisation size values are valid", {
  valid_sizes <- c("Small", "Medium", "Large", "Unknown")
  expect_true(all(breach_data$organisation_size %in% valid_sizes))
})

# Test the structure and type of the level of digital intensity
test_that("Level of digital intensity values are valid", {
  valid_intensities <- c("Low", "Low-Medium", "Medium-High", "High")
  expect_true(all(breach_data$level_of_digital_intensity %in% valid_intensities))
})


# Execute all tests
test_dir("tests")
