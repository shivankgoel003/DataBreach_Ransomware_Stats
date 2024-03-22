#### Preamble ####
# Purpose: Models different relations between variables such as mortality rates and air pollutants. 
# Author: Vanshika Vanshika, Shivank Goel, Navya Hooda
# Date: 13 March 2024
# Contact: vanshika.vanshika@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(here)
library(readr)
library(MASS)

#### Read data ####
deaths_data <- read.csv(here("data/analysis_data/cleaned_file.csv"))
air_quality_data <- read.csv(here("data/analysis_data/cleaned_air_data.csv"))
data_average <- read.csv(here("data/analysis_data/cleaned_chart_data.csv"))
data_peak <- read.csv(here("data/analysis_data/cleaned_peak_data.csv"))
alberta_cod <- merge(deaths_data, air_quality_data, by = "year")
heart <- read.csv(here("data/analysis_data/merged_heart_data.csv"))
lung <- read.csv(here("data/analysis_data/merged_data.csv"))

# Fit Poisson regression model
cause_of_death_alberta_poisson <-
  stan_glm(
    total_deaths ~ cause,
    data = alberta_cod,
    family = poisson(link = "log"),
    seed = 853
  )

# Fit Negative Binomial regression model
cause_of_death_alberta_neg_binomial <-
  stan_glm(
    total_deaths ~ cause,
    data = alberta_cod,
    family = neg_binomial_2(link = "log"),
    seed = 853
  )



# Convert year and air_quality_health_index to numeric
heart <- heart %>%
  mutate(provincial_average = as.numeric(provincial_average),
         total_deaths = as.numeric(total_deaths))
lung <- lung %>%
  mutate(provincial_average = as.numeric(provincial_average),
         total_deaths = as.numeric(total_deaths))

# Fit models

# Bayesian Negative Binomial Model with stan_glm
heart_model <- stan_glm(total_deaths ~ provincial_average, data = heart, 
                        family = neg_binomial_2(link = "log"), seed = 853)

# Poisson Regression Model
lung_model <- stan_glm(total_deaths ~ provincial_average, data = lung, 
                       family = neg_binomial_2(link = "log"), seed = 853)

# Convert 'cause' variable to factor with levels
alberta_cod$cause <- factor(alberta_cod$cause)

# Check the levels of the 'cause' variable
levels(alberta_cod$cause)


# Check the structure of the alberta_cod dataset
str(alberta_cod)
#### Save model ####
saveRDS(neg_binom_model, "models/neg_binom_model.rds")
saveRDS(stan_neg_binom_model, "models/stan_neg_binom_model.rds")
saveRDS(stan_poisson_model, "models/stan_poisson_model.rds")
saveRDS(
  cause_of_death_alberta_neg_binomial,
  file = "models/first_model.rds"
)
saveRDS(
  cause_of_death_alberta_poisson,
  file = "models/poisson_model.rds"
)

saveRDS(lung_model, "models/lung_model.rds")
saveRDS(heart_model, "models/heart_model.rds")