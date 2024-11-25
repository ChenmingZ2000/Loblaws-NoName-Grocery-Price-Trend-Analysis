#### Preamble ####
# Purpose: Model Loblaws "No Name" product prices to understand trends in grocery pricing
# Author: Chenming Zhao
# Date: 21 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - This script requires access to simulated prior distributions for grocery prices,
#     based on factors such as size, type, date, and product ID. 
#   - The `tibble`, `ggplot2`, and `dplyr` libraries should be installed.
# Overview:
#   - This script sets up prior distributions for Bayesian modeling of Loblaws "No Name" product prices.
#   - Factors influencing price include:
#     - Size (e.g., Small, Large, Extra Large),
#     - Type (e.g., Flour/Rice, Snacks),
#     - Date (capturing temporal trends),
#     - Product ID (random effects for individual products).
#   - These priors assess the model's predictions before fitting actual data, ensuring reasonable assumptions 
#     about variable ranges and distributions. Visualization of prior predictive checks provides insight 
#     into whether the priors yield realistic results.

#### Workspace Setup ####
library(tibble)
library(ggplot2)
library(dplyr)

# Set the prior parameters for simulation
set.seed(304)
draws <- 1000
num_effects <- 5

# Time range: Simulated date effect

priors <- tibble(
  sigma = rep(rexp(n = draws, rate = 1), times = num_effects),
  beta_0 = rep(rnorm(n = draws, mean = 2, sd = 5), times = num_effects),
  beta_size = rep(rnorm(n = draws, mean = 0, sd = 5), times = num_effects),
  beta_type = rep(rnorm(n = draws, mean = 0, sd = 5), times = num_effects),
  beta_date = rep(rnorm(n = draws, mean = 0, sd = 1), times = num_effects),
  beta_product = rep(rnorm(n = draws, mean = 0, sd = 1), times = num_effects),
  size = sample(c("Small", "Large", "Extra Large"), draws * num_effects, replace = TRUE),
  type = sample(c("Snacks", "Flour/Rice", "Milk", "Cooking Oil", "Seasoning"), 
                draws * num_effects, replace = TRUE),
  date = sample(seq(as.Date("2024-06-11"), as.Date("2024-11-11"), by = "month"), 
                draws * num_effects, replace = TRUE),
  product_id = sample(1:100, draws * num_effects, replace = TRUE)
) |> 
  rowwise() |> 
  mutate(
    mu = pmin(pmax(beta_0 + beta_size*beta_type + beta_date + beta_product, log(1.5)), log(50)),
    price = rgamma(n = 1, shape = exp(mu), rate = exp(mu)/10)
  )

#### Visualization ####

# Histogram of Simulated Prices
priors |>
  ggplot(aes(x = price)) +
  geom_histogram(bins = 30, fill = "#1f78b4", color = "black") +
  labs(title = "Prior Predictive Distribution of Grocery Prices",
       x = "Price ($)", y = "Frequency") +
  theme_classic()

# Average Price Trends Over Time by Size
priors |>
  group_by(date, size) |> 
  summarize(avg_price = mean(price, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = date, y = avg_price, group = size, color = size)) +
  geom_line(alpha = 0.7, size = 1) +
  labs(title = "Prior Predictive Check: Average Price Trend Over Time by Size",
       x = "Days Since Start of Study", y = "Average Price ($)", color = "Size") +
  theme_classic()

# Average Price Trends Over Time by Type
priors |>
  group_by(date, type) |> 
  summarize(avg_price = mean(price, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = date, y = avg_price, group = type, color = type)) +
  geom_line(alpha = 0.7, size = 1) +
  labs(title = "Prior Predictive Check: Average Price Trend Over Time by Type",
       x = "Days Since Start of Study", y = "Average Price ($)", color = "Type") +
  theme_classic()

# Price Distribution by Size
priors |>
  ggplot(aes(x = size, y = price, fill = size)) +
  geom_boxplot() +
  labs(title = "Prior Predictive Check: Price Distribution by Size",
       x = "Size", y = "Price ($)") +
  theme_classic() +
  theme(legend.position = "none")
