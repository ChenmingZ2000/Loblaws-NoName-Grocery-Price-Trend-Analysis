#### Preamble ####
# Purpose: Simulates a dataset of Loblaws "No Name" grocery prices with unique 
#          product IDs, sizes, types, and dates.
# Author: Chenming Zhao
# Date: 20 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` and `lubridate` packages must be installed.


#### Workspace setup ####
library(tidyverse)
library(lubridate)


#### Simulate data ####
set.seed(1234)

# Function to generate random dates
random_dates <- function() {
  seq(as.Date("2024-06-11"), as.Date("2024-11-19"), by = "day")
}

# Generate base data
base_products <- tibble(
  product_name = paste("Product", 1:10),  # 10 base products
  type = sample(c("Type 1", "Type 2", "Type 3"), size = 10, replace = TRUE)
)

# Expand data to include sizes and dates
simulated_data <- base_products %>%
  crossing(
    size = c("Small", "Large", "Extra Large"),
    date = random_dates()
  ) %>%
  mutate(
    # Assign unique numeric product_id based on product_name and size
    product_id = as.numeric(as.factor(paste(product_name, size))),
    # Generate prices based on size
    base_price = case_when(
      size == "Small" ~ runif(1, min = 2, max = 5),
      size == "Large" ~ runif(1, min = 5, max = 10),
      size == "Extra Large" ~ runif(1, min = 8, max = 15)
    ),
    # Add random fluctuation to prices
    current_price = base_price + rnorm(n(), mean = 0, sd = 0.5)
  ) %>%
  # Ensure valid prices (non-negative)
  mutate(
    current_price = if_else(current_price < 0, 0.01, round(current_price, 2))
  ) %>%
  # Randomly drop some "Extra Large" sizes
  filter(!(size == "Extra Large" & runif(n()) > 0.7))

#### Save simulated data ####
write_csv(simulated_data, "data/00-simulated_data/simulated_grocery_data.csv")

#### Preview data ####
print(simulated_data)