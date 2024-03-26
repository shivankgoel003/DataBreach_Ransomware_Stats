#### Preamble ####
# Purpose: Models different relations between variables such as cyberattack outcomes and organizational attributes. 
# Author: Shivank Goel  
# Date: [Today's Date]
# Contact: [Your Contact Information]
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(readr)

#### Read data ####
# Assuming the data file is directly in the current working directory
breach_data <- read_csv("data/analysis_data/breach_data.csv")

# Check the structure of the dataset
str(breach_data)

# Convert categorical variables to factors
breach_data$organisation_size <- as.factor(breach_data$organisation_size)
breach_data$country <- as.factor(breach_data$country)
breach_data$restructuring_after_attack <- as.numeric(breach_data$restructuring_after_attack == "Yes")

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

# Poisson regression model
poisson_model <- glm(
  number_of_users_affected ~ organisation_size + country,
  data = breach_data,
  family = poisson()
)

# Summary of the model
summary(poisson_model)

# Save the model
saveRDS(poisson_model, "models/poisson_model.rds")

# Save model
saveRDS(restructuring_model, "models/restructuring_model.rds")


