# load the required packages
library(tidyverse)

# read the HDB resale data from 2017 - 2024 into R 
# This is to ensure the accuracy of our geospatial data 
data <- read.csv("backend/raw_data/ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")

resale_price_index <- which(names(data) == "resale_price")

# Move the y variable column to the first position
data <- data[, c(resale_price_index, setdiff(1:ncol(data), resale_price_index))]

# Convert the remaining lease from "years and months" into number of years in decimal places
convert_remaining_lease <- function(data) {
  # Extract years and months using regular expressions
  years <- as.numeric(gsub(".*?(\\d+) year.*", "\\1", data$remaining_lease))
  months <- as.numeric(gsub(".*?(\\d+) month.*", "\\1", data$remaining_lease))
  # Calculate total lease in years
  total_lease_years <- years + months / 12
  # Create separate columns for years and months
  data$remaining_lease_years <- years
  data$remaining_lease_months <- months
  # Convert to numeric
  data$remaining_lease_years <- as.numeric(data$remaining_lease_years)
  data$remaining_lease_months <- as.numeric(data$remaining_lease_months)
  return(data)
}

# Apply the function to the data
data <- convert_remaining_lease(data) %>%
  mutate(remaining_lease_months = replace_na(remaining_lease_months, 0)) %>%
  mutate(remaining_lease = remaining_lease_years + remaining_lease_months/12) %>%
  select(-remaining_lease_months, -remaining_lease_years)

# create separate year and month columns
# We decided to create month dummy variables while keeping years as a continuous variable - so month is categorical and year is continuous
data <- data %>%
  separate(month, c("year", "month"), sep = "-") %>% 
  mutate(address = paste(block, street_name, sep = " "), year = as.numeric(year)) 

# convert storey_range from categorical to continuous
# For example, we will take the average of "01 TO 03" as 2 for the storey range
data <- data %>%
  separate(storey_range, into = c("lower_storey","upper_storey"), sep = " TO ") %>%
  mutate(ave_storey = as.numeric(lower_storey) + as.numeric(upper_storey)) %>%
  select(-c(lower_storey,upper_storey))

# Merge the HDB resale data with the lat_long_postal_xy dataset
lat_long_postal_xy = read.csv("backend/processed_data/lat_long_for_analysis.csv") %>% 
  mutate(postal_2dig = as.character(postal_2dig)) %>%
  select(-1)

# Merge the HDB resale set with geospatial data
data_merged <- data %>%
  left_join(lat_long_postal_xy, by = c("address" = "street")) %>%
  na.omit()

# Check for any NA values in our merged dataframe
sum(is.na(data_merged))

# Get the names of the categorical columns
get_categorical_columns <- function(data) {
  categorical_columns <- character(0)  # Initialize an empty character vector to store column names
  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  return(categorical_columns)
}

cat_columns <- data_merged %>% get_categorical_columns()

# make sure that the town, flat type, flat model, month, and hospital_1km are 
# converted to categorical variables by changing to factor class

convert_to_categorical <- function(data, char_columns) {
  for (col in char_columns) {
    if (is.character(data[[col]])) {
      data[[col]] <- as.factor(data[[col]])
    } else {
      warning(paste("Column", col, "is not a character column. Skipping conversion."))
    }
  }
  return(data)
}

data_merged <- convert_to_categorical(data_merged, cat_columns)
# Remove redundant columns from our data_merged
data_merged <- data_merged %>%
  select(-c(block,street_name,lease_commence_date,address))

# write.csv(data_merged, "backend/processed_data/hdb_merged_no_transform.csv")

#filter out the categorical variables to get a dataset that is filled 
# with only the continuous variables to allow for testing of multi-collinearity 
filter_categorical_columns <- function(data) {
  categorical_columns <- sapply(data, is.factor)
  return(data[, !categorical_columns])
}

df_continuous <- data_merged %>%
  filter_categorical_columns()

# write.csv(df_continuous, "backend/processed_data/hdb_resale_continuous.csv")

# Perform one hot encoding to create dummy variables for categorical data
one_hot_encoding <- function(data, column_names) {
  for (column_name in column_names) {
    # Get unique values in the specified column
    unique_values <- unique(data[[column_name]])
    # Create new columns for each unique value and populate with binary values
    for (value in unique_values) {
      binary_column <- as.integer(data[[column_name]] == value)
      new_column_name <- paste(column_name, value, sep = "_")
      data <- cbind(data, binary_column)
      names(data)[ncol(data)] <- new_column_name
    }
    # Remove the original column
    data <- data[, !names(data) %in% column_name]
  }
  return(data)
}

data_cleaned <- one_hot_encoding(data_merged, get_categorical_columns(data_merged))

# write.csv(data_cleaned, "backend/processed_data/hdb_resale_prices.csv")

# Get the details for each HDB block for visualisation purposes
lat_long_vis <- read.csv("backend/processed_data/lat_long_for_visualisation.csv") %>% select(-1)
data_for_vis <- data %>%
  select(c(town,lease_commence_date,address))
HDB_details <- lat_long_vis %>%
  left_join(data_for_vis, by = c("street" = "address")) %>%
  unique()
# write.csv(HDB_details, "backend/processed_data/hdb_block_details.csv")