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

# read the HDB resale data from 1990 - 2024 into R
data1 <- read.csv("backend/ResaleFlatPricesBasedonApprovalDate19901999.csv")
data2 <- read.csv("backend/ResaleFlatPricesBasedonApprovalDate2000Feb2012.csv")
data3 <- read.csv("backend/ResaleFlatPricesBasedonRegistrationDateFromMar2012toDec2014.csv")
data4 <- read.csv("backend/ResaleFlatPricesBasedonRegistrationDateFromJan2015toDec2016.csv")
data5 <- read.csv("backend/ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")

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
data5 <- convert_remaining_lease(data5) %>%
  mutate(remaining_lease_months = replace_na(remaining_lease_months, 0)) %>%
  mutate(remaining_lease = remaining_lease_years + remaining_lease_months/12) %>%
  select(-remaining_lease_months, -remaining_lease_years)

# Bind data1 to data3 as they have similar structure in their data
data1_2_3 <- rbind(data1, data2, data3) %>%mutate(date = ym(month))%>%
  mutate(lease_commence_date = ym(str_c(lease_commence_date,"01", sep = "-"))) %>%
  # To calculate remaining lease, we take 99 years HDB lease and subtract it from the difference
  # between transaction date and lease commence date
  mutate(remaining_lease = 99 - (as.numeric(date - lease_commence_date))/365.25) %>%
  select(-date) %>%
  mutate(resale_price_new = resale_price) %>%
  select(-resale_price) %>%
  rename(c("resale_price_new" = "resale_price"))

data_tidy <- rbind.fill(data1_2_3, data4, data5) 

#create separate year and month columns to create date dummy variables
data_tidy <- data_tidy %>%
  separate(month, c("year", "month"), sep = "-") %>% 
  mutate(address = paste(block, street_name, sep = " "))

# Check for any NA values in our merged dataframe
sum(is.na(data_tidy))

########################################################################################################
# forming the hdb resale data set + lat_long_postal_xy table
# read in the lat_long_postal_xy table we created
lat_long_postal_xy = read.csv("backend/lat_long_postal_xy.csv")

# Tables of nearest amenities 
nearest_mrt = read.csv("backend/nearest_mrt.csv") %>% select(-1)
nearest_supermarket = read.csv("backend/nearest_supermarket.csv") %>% select(-1)
nearest_hawkers = read.csv("backend/nearest_hawkers.csv") %>% select(-1)
nearest_primary_schools = read.csv("backend/nearest_primary_schools.csv") %>% select(-1)
nearest_hospital = read.csv("backend/nearest_hospital.csv") %>% select(-1)

# Left join the hdb block details with the nearest amenities 
# MRT, Supermarket, Hawkers, Primary Schools, Hospitals
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_mrt, by = c("address" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_supermarket, by = c("address" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_hawkers, by = c("address" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_primary_schools, by = c("address" = "Address"))
lat_long_postal_xy = left_join(lat_long_postal_xy, nearest_hospital, by = c("address" = "Address"))

# CBD long and lat
cbd_lat = 1.287953
cbd_long = 103.851784
cbd_coords = c(cbd_long,cbd_lat)

# Adding a column for distance to CBD
lat_long_postal_xy <- lat_long_postal_xy %>%
  mutate(dist_cbd = distHaversine(cbd_coords, cbind(long, lat))/1000)

# data_merged contains all the variables before creating dummies for the categorical variables
data_merged <- data_tidy %>%
  left_join(lat_long_postal_xy, by = "address") %>%
  na.omit()

# write.csv(data_merged,"backend/data_merged.csv")
########################################################################################################

#function for n - 1 binary regressors form 
# (Convert n categorical variables to (n-1) dummy variables)
convert_to_binary <- function(data, column_name) {
  # Get unique values in the specified column
  unique_values <- unique(data[[column_name]])
  # Create new columns for each unique value and populate with binary values
  for (value in unique_values) {
    binary_column <- as.integer(data[[column_name]] == value)
    data <- cbind(data, binary_column)
    names(data)[ncol(data)] <- paste(column_name, value, sep = "_")
  }
  # Remove the original column
  data <- data[, !names(data) %in% column_name]
  return(data)
}

#recode the month tab into 0 and 1, and the flat types into 0-1, and finally the town types into 0-1
data_binary <- convert_to_binary(data_tidy, "town")
data_binary <- convert_to_binary(data_binary, "flat_type")
data_binary <- convert_to_binary(data_binary, "month")
data_binary <- convert_to_binary(data_binary, "street_name")
data_binary <- convert_to_binary(data_binary, "flat_model")
data_binary <- convert_to_binary(data_binary, "flat_type")
data_binary <- convert_to_binary(data_binary, "storey_range")

# data_complete represents the successful binding of the 5 datasets as well as the conversion
# of categorical variables into dummy variables 
data_complete <- data_binary
