#### Preamble ####
# Purpose: Models different relations between variables such as cyberattack outcomes and organizational attributes. 
# Author: Shivank Goel  
# Date: 7th April 2024
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(forcats)
library(readr)
library(nnet)
library(MASS)
library(broom)

#### Read data ####
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

# Simplify the sector variable based on significance 
significant_sectors <- c("Finance and insurance", "IT and other information services", "Health activities", "Public administration and defence", "Other")
breach_data$sector_simplified <- breach_data$sector %>% 
  fct_lump(length(significant_sectors)) %>% 
  fct_relevel(significant_sectors)

# Fit the linear regression model
breach_severity_model <- lm(breach_severity_numeric ~ organisation_size + level_of_digital_intensity + sector_simplified + country_simplified, data = breach_data)

# Summary of the model
summary(breach_severity_model)

levels(breach_data$country_simplified)


############



######### Model 2

# Check the structure of the dataset
str(breach_data)



model_investigation <- lm(undertook_investigation ~ critical_industry + organisation_size +
                            level_of_digital_intensity + sector + 
                           country, data = breach_data)

# Evaluate the model
summary(model_investigation)



### Model 3 


# Convert impact_level to a factor if it's not already
breach_data$impact_on_data <- factor(breach_data$impact_on_data, levels = c("Low", "Medium", "High"))

impact_multinom_model <- multinom(impact_on_data ~ organisation_size + level_of_digital_intensity + sector_simplified + country_simplified, data = breach_data)

# Results in terms of relative risk ratios
exp(coef(impact_multinom_model))

model_summary <- summary(impact_multinom_model)


ref_level_country <- levels(breach_data$country_simplified)[1]
print(ref_level_country)

ref_level_sector <- levels(breach_data$sector_simplified)[1]
print(ref_level_sector)

ref_level_di <- levels(breach_data$level_of_digital_intensity)[1]
print(ref_level_di)


# Save the model

saveRDS(breach_severity_model,"models/breach_severity_model.rds")
saveRDS(model_investigation, "models/model_investigation.rds")
saveRDS(impact_multinom_model, "models/impact_multinom_model.rds")





