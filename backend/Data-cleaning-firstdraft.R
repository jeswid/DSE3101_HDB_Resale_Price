# firstly, load all the required packages
library(tidyverse)
library(dplyr)
library(plotly)
library(lubridate)
library(stringr)
library(readxl)
library(plyr)
library(caret)
library(httr) 
library(jsonlite)

# read the HDB resale data from 1990 - 2024 into R
data1 <- read.csv("ResaleFlatPricesBasedonApprovalDate19901999.csv")
data2 <- read.csv("ResaleFlatPricesBasedonApprovalDate2000Feb2012.csv")
data3 <- read.csv("ResaleFlatPricesBasedonRegistrationDateFromMar2012toDec2014.csv")
data4 <- read.csv("ResaleFlatPricesBasedonRegistrationDateFromJan2015toDec2016.csv")
data5 <- read.csv("ResaleflatpricesbasedonregistrationdatefromJan2017onwards.csv")

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

data_complete <- rbind.fill(data1_2_3, data4, data5) 

#create separate year and month columns to create date dummy variables
data_tidy <- data_complete %>%
  separate(month, c("year", "month"), sep = "-")

# Check for any NA values in our merged dataframe
sum(is.na(data_tidy))


##########################################################################################
# Getting the latitude and longitude of each unique address 
# geo_code <- function(address) {
#   lat <- 1.3245
#   lon <- 103.8572
#   tryCatch({
#     address = str_replace_all(address," ","%20") 
#     base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
#     endpoint <- "&returnGeom=Y&getAddrDetails=Y"
#     resource_url <- paste0(base_url,address,endpoint)
#     res <- GET(resource_url, 
#                add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
#                accept("application/json"))
#     res_list <- content(res, type = "text") %>% fromJSON(flatten = TRUE)
#     df <- as_tibble(res_list$results)[1,]
#     lat <- as.numeric(df["LATITUDE"])
#     lon <- as.numeric(df["LONGITUDE"])
#   }, error = function(e) {
#     lat <- 1.3245
#     lon <- 103.8572
#   })
#   return(c(lat, lon))
# }
# 
# data_tidy <- data_tidy %>%
#   mutate(address = paste(block, street_name, sep = ' '))
# 
# unique_addresses <- unique(data_tidy$address)
# 
# lat_long <- data.frame(
#   address = character(),
#   lat = numeric(),
#   long = numeric(),
#   stringsAsFactors = FALSE # Prevent conversion of character to factor
# )
# 
# # Now, let's iterate through the data and add rows to the data frame
# for (address in unique_addresses) {
#   coords <- geo_code(address)
#   row <- data.frame(address = address, lat = coords[1], long = coords[2], stringsAsFactors = FALSE)
#   lat_long <- bind_rows(lat_long, row)
# }
# 
# write.csv(lat_long, "backend/lat_long.csv")

##########################################################################################
# Test Example of API
# Construct the API request URL 
# base_url <- "https://www.onemap.gov.sg/api/common/elastic/search?searchVal="
# endpoint <- "&returnGeom=Y&getAddrDetails=Y"
# resource_url <- paste0(base_url,"406%20ANG%20MO%20KIO%20AVE%2010",endpoint)
# # Make the GET request with the access token in the header
# res <- GET(resource_url, 
#            add_headers(Authorization = paste("Bearer", Sys.getenv("ONEMAP_KEY"))),
#            accept("application/json"))
# 
# # Check the status code
# res$status_code
# 
# # Parse the response content
# res_list <- content(res, type = "text") %>%
#   fromJSON(flatten = TRUE)
# 
# # Convert response to tibble
# df <- as_tibble(res_list$results)
# 
# # View the data
# df
##########################################################################################

#function for n - 1 binary regressors form 
# (Convert n categorical variables to (n-1) dummy variables)
convert_to_binary <- function(data, column_name) {
  # Get unique values in the specified column
  unique_values <- unique(data[[column_name]])
  unique_values <- unique_values[-length(unique_values)]
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


########################################################################################################

#read in the lat long table we created
data_tidy <- data_tidy %>% 
  mutate(address = paste(block, street_name, sep = " "))

lat_long = read.csv("lat_long.csv") %>% select(-X)

data_latlong_merged <- data_tidy %>%
  left_join(lat_long, by = "address")

