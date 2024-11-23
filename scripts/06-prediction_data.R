#### Preamble ####
# Purpose: Predict product prices for December 15, 2024, to January 15, 2025, using a gamma model.
# Author: Chenming Zhao
# Date: 23 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites:
#   - Ensure the gamma model is saved at "models/gamma_model.rds".
#   - Load the Excel file with product details.
#   - Install the necessary libraries: `tidyverse`, `arrow`.


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
    name = factor(product_name)
  )

#### Prepare prediction data ####
# Create prediction dates
prediction_dates <- seq(as.Date("2024-12-15"), as.Date("2025-1-15"), by = "day")

# Expand product data to include all prediction dates
prediction_data <- prc_data %>%
  expand_grid(date = prediction_dates)

#### Generate predictions ####
# Use the gamma model to generate posterior predictions
prediction_samples <- posterior_predict(gamma_model, newdata = prediction_data)

# Compute predicted mean and 95% credible intervals
prediction_data <- prediction_data %>%
  mutate(
    predicted_price = apply(prediction_samples, 2, mean),
    lower_ci = apply(prediction_samples, 2, quantile, probs = 0.025),
    upper_ci = apply(prediction_samples, 2, quantile, probs = 0.975)
  )

#### Save predictions ####
write_parquet(prediction_data, "data/02-analysis_data/predicted_prices.parquet")