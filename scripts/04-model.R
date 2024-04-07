#### Preamble ####
# Purpose: Models different relations between variables such as cyberattack outcomes and organizational attributes. 
# Author: Shivank Goel  
# Date: 7th April 2024
# Contact: [Your Contact Information]
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(forcats)
library(readr)
library(MASS)
library(broom)
library(vcd)
library(caret)

#### Read data ####
# Assuming the data file is directly in the current working directory
breach_data <- read_csv("data/analysis_data/breach_data.csv")

# Convert categorical variables to factors
breach_data$country <- as.factor(breach_data$country)
breach_data$restructuring_after_attack <- as.numeric(breach_data$restructuring_after_attack == "Yes")
breach_data$organisation_size <- as.factor(breach_data$organisation_size)
breach_data$sector <- as.factor(breach_data$sector)
breach_data$critical_industry <- as.factor(breach_data$critical_industry)
breach_data$level_of_digital_intensity <- as.factor(breach_data$level_of_digital_intensity)
breach_data$number_of_users_affected <- as.numeric(breach_data$number_of_users_affected)



breach_data$critical_industry <- ifelse(breach_data$critical_industry == "Yes", 1, 0)
breach_data$cyber_security_role <- ifelse(breach_data$cyber_security_role == "Yes", 1, 0)
breach_data$undertook_investigation <- ifelse(breach_data$undertook_investigation == "Yes", 1, 0)


######### Model 1

calculate_breach_severity <- function(impact, credit_card_leak_1, credit_card_leak_2, ssn_leak, fraudulent_use) {
  if (is.na(impact) || is.na(credit_card_leak_1) || is.na(credit_card_leak_2) || is.na(ssn_leak) || is.na(fraudulent_use)) {
    return(NA)  # Return NA if any of the inputs is NA
  } else if (impact == 'High' || fraudulent_use == 'Yes') {
    return('High')
  } else if ((credit_card_leak_1 == 'Yes' || credit_card_leak_2 == 'Yes' || ssn_leak == 'Yes') && impact != 'Low') {
    # If any credit card or SSN data is leaked, and impact is not 'Low', consider it 'Medium' severity
    return('Medium')
  } else {
    return('Low')
  }
}

# Apply the function to each row in the dataframe
breach_data$breach_severity <- mapply(calculate_breach_severity,
                                      breach_data$aspect_of_confidentiality_integrity_availability_triad_affected,
                                      breach_data$track_1_credit_card_details_leaked_exposed,
                                      breach_data$track_2_credit_card_details_leaked_exposed,
                                      breach_data$social_security_number_tax_number_leaked_exposed,
                                      breach_data$subsequent_fraudulent_use_of_data)


# Fit the linear regression model using the dataframe without NAs
breach_data$breach_severity_numeric <- as.numeric(factor(breach_data$breach_severity, levels = c("Low", "Medium", "High")))

# Select the independent variables and convert categorical variables to factors
breach_data$organisation_size <- as.factor(breach_data$organisation_size)
breach_data$level_of_digital_intensity <- as.factor(breach_data$level_of_digital_intensity)
breach_data$sector <- as.factor(breach_data$sector)
breach_data$country <- as.factor(breach_data$country)

# Check unique values in the country variable
unique_countries <- unique(breach_data$country)

# Simplify the country variable
breach_data$country_simplified <- breach_data$country %>% 
  fct_lump(5) %>%    # This keeps the top 5 countries and lumps the rest into "Other"
  fct_relevel("USA", "Australia", "Canada", "Global", "Japan", "UK", "Other")

# Check unique values in the sector variable
unique_sectors <- unique(breach_data$sector)

# Simplify the sector variable based on significance (you will need domain knowledge or statistical tests to decide which ones to keep)
significant_sectors <- c("Finance and insurance", "IT and other information services", "Health activities", "Public administration and defence", "Other")
breach_data$sector_simplified <- breach_data$sector %>% 
  fct_lump(length(significant_sectors)) %>% 
  fct_relevel(significant_sectors)

# Next, we fit the linear regression model
# (you might need to modify this formula based on the actual predictors you wish to include)
breach_severity_model <- lm(breach_severity_numeric ~ organisation_size + level_of_digital_intensity + sector_simplified + country_simplified, data = breach_data)

# Finally, let's look at the summary of the model to interpret the results
summary(breach_severity_model)

############



######### Model 2

# Check the structure of the dataset
str(breach_data)



model_investigation <- lm(undertook_investigation ~ critical_industry + organisation_size +
                            level_of_digital_intensity + sector + 
                           country, data = breach_data)

# Evaluate the model
summary(model_investigation)



######### Model 3
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


###### Model 4
yearly_attacks <- breach_data %>%
  group_by(year) %>%
  summarise(number_of_attacks = n())

yearly_data <- breach_data %>%
  group_by(year) %>%
  summarise(
    number_of_attacks = n(), sector,
    number_of_users_affected = sum(number_of_users_affected, na.rm = TRUE))


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
######

########
# For linear regression with interaction terms
interaction_model <- lm(number_of_attacks ~ year * sector, data = yearly_data)

######
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

######

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

######

#### Fit Negative Binomial Model with stan_glm ####
cyberattack_neg_binomial <- stan_glm(
  number_of_attacks ~  sector + organisation_size, 
  data = aggregated_data, 
  family = neg_binomial_2(link = "log"), 
  seed = 853
)




linear_model_RQ2 <- lm(number_of_users_affected ~ sector + organisation_size + critical_industry + level_of_digital_intensity + cyber_security_role + cyber_security_frameworks, data = breach_data)
summary(linear_model_RQ2)

# Example for RQ3 focusing on different aspects
linear_model_RQ3 <- lm(number_of_users_affected ~ country + policy + prevention_detection_and_recovery + detector + restructuring_after_attack, data = breach_data)
summary(linear_model_RQ3)


# Save the model

saveRDS(breach_severity_model,"models/breach_severity_model")
saveRDS(cyberattack_poisson, "models/cyberattack_poisson.rds")
saveRDS(cyberattack_neg_binomial , "models/cyberattack_neg_binomial.rds")
saveRDS(linear_model, "models/linear_model.rds")
saveRDS(restructuring_model, "models/restructuring_model.rds")
saveRDS(linear_model_RQ2, "models/linear_model_RQ2.rds")
saveRDS(linear_model_RQ3, "models/linear_model_RQ3.rds")
saveRDS(model_investigation, "models/model_investigation.rds")






