#### Preamble ####
# Purpose: This script models product prices based on size, type, and time trends 
#          using Bayesian hierarchical modeling.
# Author: Chenming Zhao
# Date: 21 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - This script requires the `tidyverse`, `rstanarm`, and `arrow` libraries 
#     for data manipulation and Bayesian modeling.
#   - Ensure that `loblaws_analysis_data.parquet` is available in the specified path 
#     "/cloud/project/data/02-analysis_data/".
#   - The output model will be saved as `price_size_type_model.rds` in the `models` folder.
# Any other information needed? None

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)

#### Read data ####
model_loblaws_data <- read_parquet("data/02-analysis_data/loblaws_data.parquet")

#### Prepare data ####

# Ensure product_size and product_type are factors
model_loblaws_data <- model_loblaws_data %>%
  mutate(
    size = factor(size, levels = c("Small", "Large", "Extra Large")),
    type = factor(type),
    date = as.Date(date)
  )

### Model data ####

# Bayesian hierarchical model for product prices

price_model <- stan_glmer(
  current_price ~ size * type + date + (1 | product_id),
  data = loblaws_data,
  family = Gamma(link = "log"),
  prior = normal(0, 5, autoscale = TRUE),
  prior_intercept = normal(2, 5, autoscale = TRUE),
  iter = 1000, 
  chains = 4,      
  cores = 4
)

summary(price_model)

#### Save model ####
saveRDS(
  price_model,
  file = "models/price_model.rds"
)