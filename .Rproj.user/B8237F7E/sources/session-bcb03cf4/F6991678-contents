#### Preamble ####
# Purpose: Predict product prices from now to Februray 15, 2025, using a gamma model.
# Author: Chenming Zhao
# Date: 23 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites:
#   - Ensure the gamma model is saved at "models/price_model.rds".
#   - Load the Excel file with product details.
#   - Install the necessary libraries: `tidyverse`, `arrow`, `rstanarm`, `brms`.


#### Workspace setup ####
library(tidyverse)
library(arrow)
library(rstanarm)
library(brms)

# Load Data
lbl <- read_parquet("data/02-analysis_data/loblaws_data.parquet")
gamma_model <- readRDS("models/price_model.rds")

# Read product data from Excel file
prc_data <- lbl %>%
  select(product_id,product_name,size,type) %>%
  distinct()

#### Check and prepare input data ####
# Ensure required columns are present
required_cols <- c("product_id", "size", "type", "product_name")
if (!all(required_cols %in% colnames(prc_data))) {
  stop("The Excel file must contain the columns: product_id, size, type, and name.")
}

# Ensure categorical variables are factors
prc_data <- prc_data %>%
  mutate(
    size = factor(size, levels = c("Small", "Large", "Extra Large")),
    type = factor(type, levels = unique(type)),
    product_id = factor(product_id),
    product_name = factor(product_name)
  )

#### Prepare prediction data ####
# Create prediction dates
prediction_dates <- seq(as.Date("2024-11-20"), as.Date("2025-2-15"), by = "day")

# Expand product data to include all prediction dates
prediction_data <- prc_data %>%
  expand_grid(date = prediction_dates)

#### Generate predictions ####
set.seed(123)
prediction_samples <- posterior_predict(gamma_model, newdata = prediction_data)

# Compute predicted mean and 95% credible intervals
prediction_data <- prediction_data %>%
  mutate(
    predicted_price = apply(prediction_samples, 2, mean),
    lower_ci = apply(prediction_samples, 2, quantile, probs = 0.025),
    upper_ci = apply(prediction_samples, 2, quantile, probs = 0.975)
  )

# Add a group identifier for 14-day intervals
prediction_data <- prediction_data %>%
  mutate(
    date_group = as.integer(as.Date(date) - min(as.Date(date))) %/% 14 + 1
  )

# Generate random increases for each group
group_adjustments <- prediction_data %>%
  distinct(date_group, product_id) %>%
  mutate(random_increase = runif(n(), min = 0.1, max = 0.3))

# Join the group adjustments back to the prediction data
prediction_data <- prediction_data %>%
  left_join(group_adjustments, by = c("date_group", "product_id")) %>%
  mutate(
    # Apply the group-specific random increase
    adjusted_price = round(predicted_price * (1 + random_increase), 2),
    adjusted_lower_ci = round(lower_ci * (1 + random_increase), 2),
    adjusted_upper_ci = round(upper_ci * (1 + random_increase), 2)
  )

#### Save predictions ####
write_parquet(prediction_data, "data/02-analysis_data/predicted_prices.parquet")
