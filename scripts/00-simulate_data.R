#### Preamble ####
# Purpose: Cleans the two data sets for the years 2018 to 2021
# Author: Navya Hooda, Shivank Goel, Vanshika Vanshika
# Date: 07 March 2024 
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

# Load the tidyverse package for data manipulation
library(tidyverse)
library(MASS)

# Set seed for reproducibility
set.seed(2024)

#### Simulate death data ####

# Since we have 4 diseases and 12 years, each disease should appear exactly 12 times to match the number of years
death_data <- tibble(
  year = rep(2011:2022, times = 4),  # Replicate each year 4 times to match the number of diseases
  disease = rep(c("Other chronic obstructive pulmonary disease", 
                  "All other forms of chronic ischemic heart disease", 
                  "Acute myocardial infarction", 
                  "Malignant neoplasms of trachea, bronchus and lung"), each = 12),  # Each disease repeated 12 times
  deaths = rnbinom(n = 48, size = 100, prob = 0.02)  # We now need 48 random numbers to match the expanded dataset
)

#### Simulate air quality data ####

# Assuming the PM2.5 data is the same for both locations, we replicate the PM2.5 data twice for each year
pm25_data <- tibble(
  year = rep(2011:2022, times = 2),  # Each year appears twice
  pm25 = rnbinom(n = 24, size = 50, prob = 0.05)  # We now need 24 random numbers to match the number of years times two
)

#### Combine both datasets for modeling ####

# Join datasets by 'year'
combined_data <- left_join(death_data, pm25_data, by = "year")

head(combined_data)

# Fit a Negative Binomial Model
model <- glm.nb(deaths ~ pm25 + factor(year) + factor(disease), data = combined_data)

# Model Diagnostics
par(mfrow=c(2,2))
plot(model)

# Plotting Observed vs. Predicted Deaths
combined_data$predicted_deaths <- predict(model, type = "response")
ggplot(combined_data, aes(x = deaths, y = predicted_deaths)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Observed Deaths", y = "Predicted Deaths") +
  theme_minimal()

# Effect of PM2.5 on Predicted Deaths
ggplot(combined_data, aes(x = pm25, y = predicted_deaths, color = factor(year))) +
  geom_point() +
  labs(x = "PM2.5 Concentration", y = "Predicted Deaths") +
  theme_minimal() +
  facet_wrap(~disease)
