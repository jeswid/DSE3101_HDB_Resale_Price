# firstly, load all the required packages
library(tidyverse)
library(plotly)
library(lubridate)
library(stringr)
library(readxl)
library(plyr)
library(caret)
library(httr) 
library(jsonlite)
library(geosphere)

# read the HDB resale data from 2017 - 2024 into R 
# This is to ensure the accuracy of our geospatial data 
data <- read.csv("backend/raw_data/ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")

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

#create separate year and month columns to create date dummy variables
data_tidy <- data %>%
  separate(month, c("year", "month"), sep = "-") %>% 
  mutate(address = paste(block, street_name, sep = " "))

# Check for any NA values in our merged dataframe
sum(is.na(data_tidy))
