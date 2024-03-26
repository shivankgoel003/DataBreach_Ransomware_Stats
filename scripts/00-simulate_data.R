#### Preamble ####
# Purpose: Simulates the cyberattack dataset for analysis on breaches from 2010 to 2020
# Author:  Shivank Goel
# Date: 27 March 2024 
# Contact: shivankg.goel@mail.utoronto.ca
# License: MIT

# Load the tidyverse package for data manipulation
library(tidyverse)
library(MASS)
# Set seed for reproducibility
set.seed(2024)

#### Simulate cyberattack data ####

# Simulating the number of organizations affected by cyberattacks for 5 years
# We'll assume the number of affected organizations follows a negative binomial distribution

# Defining variables
years <- 2016:2020
organization_sizes <- c("Small", "Medium", "Large", "NA")
levels_of_digital_intensity <- c("Low", "Medium", "High", "NA")
sectors <- c("Healthcare", "Finance", "Education", "Retail", "NA")

# Simulating the data
cyberattack_data <- expand.grid(year = years,
                                organization_size = organization_sizes,
                                level_of_digital_intensity = levels_of_digital_intensity,
                                sector = sectors) %>%
  mutate(number_of_attacks = rnbinom(n = nrow(.), size = 1, mu = 10)) # average number of attacks is 10

#### Examine the simulated data ####

head(cyberattack_data)

#### Statistical analysis ####

# Fit a Negative Binomial Model to predict the number of attacks
# based on organization size, level of digital intensity, and sector
model <- glm.nb(number_of_attacks ~ organization_size + level_of_digital_intensity + sector,
                data = cyberattack_data)

# Model summary
summary(model)

#### Visualization ####

# Plotting Number of Attacks by Sector
ggplot(cyberattack_data, aes(x = sector, y = number_of_attacks, fill = organization_size)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Number of Cyberattacks by Sector and Organization Size",
       x = "Sector", y = "Number of Attacks") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
