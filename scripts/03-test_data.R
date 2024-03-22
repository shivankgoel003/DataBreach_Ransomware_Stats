#### Preamble ####
# Purpose: Data Validation and Testing for Mortality and Air Quality Data Analysis
# Authors: Navya Hooda, Shivank Goel, Vanshika Vanshika
# Date: 12th February 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, testthat

#### Workspace setup ####
library(tidyverse)
library(testthat)


#Unit Testing Reference:https://testthat.r-lib.org/reference/test_that.html 

# Load the datasets
cleaned_air_data <- read.csv("data/analysis_data/cleaned_air_data.csv")
cleaned_avg_pm25_conc <- read.csv("data/analysis_data/cleaned_avg_pm25_conc.csv")
cleaned_mortality_data <- read.csv("data/analysis_data/cleaned_file.csv")

#### Test data ####

# Test the structure and type of the air quality data
test_that("Air quality data columns are of the correct type", {
  expect_type(cleaned_air_data$year, "integer")
  expect_type(cleaned_air_data$health_risk, "character")
  expect_type(cleaned_air_data$original_value, "double")
})

# Test the structure and type of the PM2.5 concentration data
test_that("PM2.5 concentration data columns are of the correct type", {
  expect_type(cleaned_avg_pm25_conc$years, "integer")
  expect_type(cleaned_avg_pm25_conc$provincial_average, "double")
  expect_type(cleaned_avg_pm25_conc$x10th_percentile, "double")
  expect_type(cleaned_avg_pm25_conc$x90th_percentile, "double")
})

# Test the structure and type of the mortality data
test_that("Mortality data columns are of the correct type", {
  expect_type(cleaned_mortality_data$year, "integer")
  expect_type(cleaned_mortality_data$cause, "character")
  expect_type(cleaned_mortality_data$ranking, "integer")
  expect_type(cleaned_mortality_data$total_deaths, "integer")
})

# Test that the years are within the expected range
test_that("Year values are within the expected range", {
  expect_true(all(cleaned_air_data$year >= 2012 & cleaned_air_data$year <= 2022))
  expect_true(all(cleaned_avg_pm25_conc$years >= 2012 & cleaned_avg_pm25_conc$years <= 2022))
  expect_true(all(cleaned_mortality_data$year >= 2012 & cleaned_mortality_data$year <= 2022))
})

# Test if health risk values are one of the expected categories
test_that("Health risk values are valid", {
  valid_health_risks <- c("High Quality", "Moderate Quality", "Low Quality", "Very Low Quality")
  expect_true(all(cleaned_air_data$health_risk %in% valid_health_risks))
})

# Test if total deaths are all positive
test_that("Total deaths values are positive", {
  expect_true(all(cleaned_mortality_data$total_deaths > 0))
})

# Test for missing values
test_that("There are no missing values in key columns", {
  expect_true(all(!is.na(cleaned_air_data$air_quality_health_index)))
  expect_true(all(!is.na(cleaned_avg_pm25_conc$provincial_average)))
  expect_true(all(!is.na(cleaned_mortality_data$total_deaths)))
})

