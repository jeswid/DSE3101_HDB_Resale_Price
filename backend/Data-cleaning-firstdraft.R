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

data_original <- rbind.fill(data1, data2, data3, data4, data5)

#create separate year and month columns
data_transformed <- data_original %>% mutate(date = month) %>%
  select(-month) %>%
  separate(date, c("year", "month"), sep = "-")

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
