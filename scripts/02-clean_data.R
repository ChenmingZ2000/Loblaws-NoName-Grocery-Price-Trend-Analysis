# Purpose: Analyze and clean polling and product pricing data for Loblaws "No Name" brand, focusing on specific 
#          products and their price trends across different types and units.
# Author: Chenming Zhao
# Date: 20 November 2024
# Contact: chenming.zhao@mail.utoronto.ca
# License: MIT
# Pre-requisites:
#    - Download the raw polling data from https://jacobfilipp.com/hammer/
#    - Install necessary R libraries: dplyr, tidyverse, stringr, arrow (for parquet saving).
#    - Save the raw data files in an accessible directory, or adjust file paths in the script as needed.
#    - Basic knowledge of R, data wrangling, and type mapping for analysis.
# Key Notes:
#    - The analysis filters data for the "No Name" brand sold by Loblaws and focuses on specific product types.
#    - This script processes time-series data to retain the most recent prices for products on each date.
#    - Output is saved in a parquet format for optimized storage and analysis.


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(stringr)
library(arrow)

#### Clean data ####

# Read the data files
# run gunzip -c data/01-raw_data/hammer-4-raw.csv.gz > data/01-raw_data/hammer-4-raw.csv in terminal
hammer4raw <- read_csv("data/01-raw_data/hammer-4-raw.csv")
hammer4product <- read_csv("data/01-raw_data/hammer-4-product.csv")

# Filter to include only Loblaws products from the "No Name" brand
product_data <- hammer4product %>%
  filter(vendor %in% c("Loblaws"), brand %in% c("No Name"))

# Clean the product names by removing "Club Size" from the end of product_name
processed_data <- product_data %>%
  mutate(
    category = str_remove(product_name, "\\+Club Size$")
  )

# Filter products that appear at least twice, keeping all related products (e.g., different sizes)
filtered_data <- processed_data %>%
  group_by(product_name) %>%
  filter(n() >= 2) %>%
  ungroup()

# Merge product metadata with raw pricing data using product_id
merged_data <- hammer4raw %>%
  inner_join(filtered_data, by = c("product_id" = "id"))

# Remove rows where current_price is NA and select relevant columns
cleaned_data <- merged_data %>%
  filter(!is.na(current_price)) %>%
  select(product_id, nowtime, current_price, price_per_unit, product_name, units)

# For each product, keep only the latest price recorded on each date
latest_data <- cleaned_data %>%
  group_by(product_id, date = as.Date(nowtime)) %>%
  slice_max(order_by = nowtime, n = 1) %>% 
  ungroup() %>%
  select(product_id, date, current_price, price_per_unit, product_name, units)

# Filter to retain only products with multiple units on the same date
filtered_latest_data <- latest_data %>%
  group_by(date, product_name) %>%              
  filter(n_distinct(units) > 1) %>%              
  ungroup()

# Extract all unique product names for further mapping
products <- filtered_latest_data %>%
  distinct(product_name, units, product_id)

# Create a mapping of product types to specific product names
size_mapping <- tibble(
  size = c("Small", "Large", "Extra Large"),
  product_id = c(
    "1869961, 1870037, 1870410, 1870564, 1870652, 1871107, 1871389, 1295620, 
    1872468, 1872517, 1872867, 1873066, 1873102, 1873150, 1873364, 1873513, 
    1873878, 1874067, 1874144, 1874570, 1874884, 1875230, 1875879, 1875989, 
    1876097, 1876368, 1876744, 1877397, 1878731, 1878741, 1878744, 1879141, 
    1879171, 1879204, 1879227, 1879319, 1879684, 1880380, 1881397, 1305400, 
    1881588, 1881677, 1881843, 1882147, 1882823, 1882933, 1883859, 1884272,
    1884293, 1884443, 1884491, 1884636, 1884640, 1884810, 1885668, 1886195,
    1886578, 1886621, 1886675, 1886944, 1887008, 1887114, 1887173, 1887214",
    "1869962, 1870035, 1870044, 1870066, 1870068, 1870413, 1870562, 1870651, 
    1871108, 1871388, 1295619, 1872467, 1872516, 1872869, 1873065, 1873098, 
    1873151, 1873363, 1873514, 1873877, 1874068, 1874139, 1874571, 1874885, 
    1875229, 1875880, 1875988, 1876096, 1876369, 1876747, 1877399, 1878732, 
    1878740, 1878747, 1879140, 1879170, 1879206, 1879225, 1757033, 1879686, 
    1880382, 1881396, 1305401, 1881589, 1881676, 1881841, 1882148, 1882822, 
    1882931, 1883860, 1884271, 1884292, 1884444, 1884492, 1884634, 1884638, 
    1884809, 1885669, 1886197, 1886579, 1886622, 1886676, 1886943, 1887006, 
    1887113, 1887174, 1887213",
    "1870038, 1870043, 1870067, 1870070, 1870411, 1870412, 1875228, 1878737, 
    1878739, 1878746, 1879205, 1879228, 1879318, 1879687, 1880381, 1375189, 
    1882932, 1886198, 1310552, 1887172, 1311190"
  )
) %>%
  mutate(product_id = str_split(product_id, ",\\s*")) %>%
  unnest(product_id)

# Create a mapping of product types to specific product names
type_mapping <- tibble(
  type = c("Snacks", "Milk", "Seasoning", "Cooking Oil", "Flour/Rice"),
  product_name = c(
    "Blueberry Waffles, Original Waffles, Microwave Butter Flavour Popcorn, Crispy Rice Marshmallow Squares, Salted Tops Soda Crackers",
    "Chocolate Ice Milk, Pasteurized Instant Skim Milk Powder, Vanilla Ice Milk",
    "Black Peppercorns, Chili Powder, Garlic Salt, Ground Black Pepper, Parsley Flakes",
    "100% Pure Canola Oil, 100% Pure Corn Oil, 100% Pure Sunflower Oil, 100% Pure Vegetable Oil",
    "Unbleached All-Purpose Flour, Whole Wheat Flour, All-Purpose Flour, Long Grain Brown Rice, Long Grain Parboiled Rice, Long Grain White Rice, Medium Grain Calrose Rice"
  )
) %>%
  mutate(product_name = str_split(product_name, ",\\s*")) %>%
  unnest(product_name)

# Merge the filtered data with type mapping to categorize products
loblaws_data <- filtered_latest_data %>%
  inner_join(type_mapping, by = c("product_name" = "product_name"))

# Convert `product_id` to character in both datasets
loblaws_data <- loblaws_data %>%
  mutate(product_id = as.character(product_id))

size_mapping <- size_mapping %>%
  mutate(product_id = as.character(product_id))

loblaws_data <- loblaws_data %>%
  mutate(current_price = as.double(current_price))

loblaws_data <- loblaws_data %>% 
  inner_join(size_mapping, by = c("product_id" = "product_id"))

# Compare the price trends of different products to see if there are obvious random effects.
ggplot(loblaws_data, aes(x = as.factor(product_id), y = current_price)) +
  geom_boxplot() +
  labs(x = "Product ID", y = "Price", title = "Price Distribution by Product ID") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Plot box plots of prices by size and type
ggplot(loblaws_data, aes(x = size, y = current_price, fill = type)) +
  geom_boxplot() +
  labs(x = "Size", y = "Price", title = "Price Distribution by Size and Type") +
  theme_minimal()


# Remove all products that have no record before September
products_with_data_before_september <- loblaws_data %>%
  filter(lubridate::month(date) < 9) %>%
  distinct(product_id)

loblaws_data <- loblaws_data %>%
  filter(product_id %in% products_with_data_before_september$product_id)

#### Save data ####
write_parquet(loblaws_data, "data/02-analysis_data/loblaws_data.parquet")
