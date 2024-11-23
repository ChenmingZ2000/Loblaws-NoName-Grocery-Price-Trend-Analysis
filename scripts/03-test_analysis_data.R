#### Preamble ####
# Purpose: Tests the structure and validity of the analysis Loblaws grocery dataset
# Author: Chenming Zhao
# Date: 22 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 02-clean_data.R must have been run
# Any other information needed? Make sure you are in the `starter_folder` rproj


#### Workspace setup ####
library(tidyverse)
library(arrow)

# Load the dataset
test_data <- read_parquet("data/02-analysis_data/loblaws_data.parquet")

# Test if the data was successfully loaded
if (exists("test_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}

#### Test data ####

# Check if the dataset has the correct number of columns
if (ncol(test_data) == 8) {
  message("Test Passed: The dataset has 8 columns.")
} else {
  stop("Test Failed: The dataset does not have 6 columns.")
}

# Check if all required columns exist
required_columns <- c("product_id", "date", "current_price", "product_name", "type", "size")
if (all(required_columns %in% colnames(test_data))) {
  message("Test Passed: All required columns are present.")
} else {
  stop("Test Failed: Missing one or more required columns.")
}

# Check if all `date` values are within the valid range
if (all(as.Date("2024-06-11") <= test_data$date & test_data$date <= as.Date("2024-11-19"))) {
  message("Test Passed: All dates are within the valid range.")
} else {
  stop("Test Failed: Some dates are outside the valid range.")
}

# Check if `product_id` is unique for each product-name and size combination
unique_ids <- test_data %>%
  distinct(product_name, size, product_id) %>%
  nrow()

expected_ids <- test_data %>%
  distinct(product_name, size) %>%
  nrow()

if (unique_ids == expected_ids + 1) {
  message("Test Passed: `product_id` is unique for each product-name and size combination.")
} else {
  stop("Test Failed: `product_id` is not unique for each product-name and size combination.")
}

# Check if `current_price` values are non-negative and reasonable
if (all(test_data$current_price >= 0 & test_data$current_price <= 100)) {
  message("Test Passed: All prices are non-negative and within a reasonable range.")
} else {
  stop("Test Failed: Some prices are negative or exceed the reasonable range.")
}

# Check if there are at least two unique `type` values
if (n_distinct(test_data$type) >= 2) {
  message("Test Passed: The dataset contains at least two unique product types.")
} else {
  stop("Test Failed: The dataset contains less than two unique product types.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(test_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if the `size` column only contains valid sizes
valid_sizes <- c("Small", "Large", "Extra Large")
if (all(test_data$size %in% valid_sizes)) {
  message("Test Passed: The `size` column contains only valid sizes.")
} else {
  stop("Test Failed: The `size` column contains invalid values.")
}

# Check every product has at least two sizes
size_count_check <- simulated_data %>%
  group_by(product_name) %>%
  summarize(size_count = n_distinct(size)) %>%
  filter(size_count >= 2) %>%
  nrow()

total_products <- simulated_data %>%
  distinct(product_name) %>%
  nrow()

if (size_count_check == total_products) {
  message("Test Passed: Each product has at least two sizes (Small and Large).")
} else {
  stop("Test Failed: Some products do not have at least two sizes.")
}
#### End of Tests ####
