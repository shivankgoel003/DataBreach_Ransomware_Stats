#### Preamble ####
# Purpose: Models different relations between variables such as cyberattack outcomes and organizational attributes. 
# Author: Shivank Goel  
# Date: [Today's Date]
# Contact: [Your Contact Information]
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(readr)
library(MASS)

#### Read data ####
# Assuming the data file is directly in the current working directory
breach_data <- read_csv("data/analysis_data/breach_data.csv")

# Check the structure of the dataset
str(breach_data)

# Convert categorical variables to factors
breach_data$country <- as.factor(breach_data$country)
breach_data$restructuring_after_attack <- as.numeric(breach_data$restructuring_after_attack == "Yes")
breach_data$organisation_size <- as.factor(breach_data$organisation_size)
breach_data$sector <- as.factor(breach_data$sector)
breach_data$critical_industry <- as.factor(breach_data$critical_industry)
breach_data$level_of_digital_intensity <- as.factor(breach_data$level_of_digital_intensity)


# Fit logistic regression model
restructuring_model <-
  stan_glm(
    restructuring_after_attack ~ organisation_size + country,
    data = breach_data,
    family = binomial(link = "logit"),
    seed = 853
  )

# Summary of the model
summary(restructuring_model)



yearly_attacks <- breach_data %>%
  group_by(year) %>%
  summarise(number_of_attacks = n())

yearly_data <- breach_data %>%
  group_by(year) %>%
  summarise(
    number_of_attacks = n(), sector
    number_of_users_affected = sum(number_of_users_affected, na.rm = TRUE)
    # You can add more summary stats if needed
  )

library(forecast)

# Assuming 'number_of_attacks' is aggregated by year
ts_data <- ts(yearly_data$number_of_attacks, start=min(yearly_data$year), frequency=1)

# Fit ARIMA model
arima_model <- auto.arima(ts_data)

# Future predictions
future_values <- forecast(arima_model, h=5)  # Forecast next 5 years

# Plot
autoplot(future_values)


trend_model <- lm(number_of_users_affected ~ year, data = yearly_data)


# For linear regression with interaction terms
interaction_model <- lm(number_of_attacks ~ year * sector, data = yearly_data)

# For separate time series in each sector
sector_ts_models <- breach_data %>%
  group_by(sector) %>%
  do(trend_model = auto.arima(.$number_of_users_affected))


# Plotting the trend of total number of users affected over the years
ggplot(yearly_data, aes(x = year, y = number_of_users_affected)) +
  geom_line() +
  geom_point() +
  labs(title = "Yearly Trend of Number of Users Affected by Cyberattacks",
       x = "Year",
       y = "Total Number of Users Affected")


# Linear regression model
lrmodel <- lm(number_of_attacks ~ year, data = yearly_attacks)

# Save the model
saveRDS(lrmodel, "models/lrmodel.rds")

# Fit a Bayesian linear regression model
linear_model <- stan_glm(
  number_of_users_affected ~ year, 
  data = breach_data, 
  family = gaussian(),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  seed = 853
)


# Aggregate data to count number of attacks by year, sector, and organisation size
aggregated_data <- breach_data %>%
  group_by(sector, organisation_size) %>%
  summarise(number_of_attacks = n(), .groups = 'drop')

#### Fit Poisson Model with stan_glm ####
cyberattack_poisson <- stan_glm(
  number_of_attacks ~  sector + organisation_size, 
  data = aggregated_data, 
  family = poisson(link = "log"), 
  seed = 853
)

#### Fit Negative Binomial Model with stan_glm ####
cyberattack_neg_binomial <- stan_glm(
  number_of_attacks ~  sector + organisation_size, 
  data = aggregated_data, 
  family = neg_binomial_2(link = "log"), 
  seed = 853
)




# Save the model
saveRDS(cyberattack_poisson, "models/cyberattack_poisson.rds")
 

# Save the model
saveRDS(cyberattack_neg_binomial , "models/cyberattack_neg_binomial.rds")


# Save the model
saveRDS(linear_model, "models/linear_model.rds")

# Save model
saveRDS(restructuring_model, "models/restructuring_model.rds")




