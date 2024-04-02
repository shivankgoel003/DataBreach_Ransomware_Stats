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
  number_of_attacks ~ year + sector + organisation_size, 
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




