#firstly, load all the required packages
library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(readxl)
library(plyr)

#read the data into R
data2 <- read.csv("ResaleFlatPricesBasedonApprovalDate2000Feb2012.csv")
data1 <- read.csv("ResaleFlatPricesBasedonApprovalDate19901999.csv")
data4 <- read.csv("ResaleFlatPricesBasedonRegistrationDateFromJan2015toDec2016.csv")
data5 <- read.csv("ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")
data3 <- read.csv("ResaleFlatPricesBasedonRegistrationDateFromMar2012toDec2014.csv")

#bind all the data into 1

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
data5_new <- convert_remaining_lease(data5) %>%
  mutate(remaining_lease_months = replace_na(remaining_lease_months, 0)) %>%
  mutate(remaining_lease = remaining_lease_years + remaining_lease_months/12) %>%
  select(-remaining_lease_months, -remaining_lease_years)

data5 <- data5_new

data1_2_3 <- rbind(data1, data2, data3) %>%mutate(date = ym(month))%>%
  mutate(lease_commence_date = ym(str_c(lease_commence_date,"01", sep = "-"))) %>%
  mutate(remaining_lease = 99 - (as.numeric(date - lease_commence_date))/365.25) %>%
  select(-date) %>%
  mutate(resale_price_new = resale_price) %>%
  select(-resale_price) %>%
  rename(resale_price = resale_price_new)


data_original <- rbind.fill(data1, data2, data3, data4, data5)

#create separate year and month columns
data_transformed <- data_original %>% mutate(date = ym(month)) %>%
  separate(month, c("year", "month"), sep = "-")



#function for 0-1 recoding:
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
data_binary_town <- convert_to_binary(data_tidy, "town")
data_binary_town_flat <- convert_to_binary(data_binary_town, "flat_type")
data_binary_town_flat_month <- convert_to_binary(data_binary_town, "month")
