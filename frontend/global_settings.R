# library('rgdal')
# library('spdplyr')

library('parsnip')
library('leaflet')
library("xgboost")
library('htmltools')
library('DT')
library('plyr')
library('dplyr')
library('tidyr')
library('ggplot2')
library("htmltools")
library('shiny')
library('shinydashboard')
library('leaflet')
library('RColorBrewer')
library(sf)
library("plotly")

############# MAP DATA ###############
SG_map = readRDS("frontend/data/SG_map.rds")
all_address = read.csv("frontend/data/unique_hdb_block_details_w_schools_mrt_name.csv") %>%
  rename(lng = long) 
amenities = read.csv("frontend/data/lat_long_for_visualisation.csv")
hawker_centres = read.csv("frontend/data/hawker_centres_geocode.csv")
hospitals = read.csv("frontend/data/hospitals_geocode.csv")
mrt = read.csv("frontend/data/hospitals_geocode.csv")
pri_schs = read.csv("frontend/data/primary_schools_geocode.csv")
supermarkets = read.csv("frontend/data/supermarkets_geocode.csv")


############# PREDICTION DATA ###############
laty = readRDS("frontend/data/lat_long_for_prediction.Rds")
all_address_pred = readRDS("frontend/data/hdb_merged_no_transform.Rds")
sample_df = readRDS("frontend/data/sample_obs_inputs.Rds")
model = readRDS("frontend/xgb.rds")

############# FUNCTIONS REQUIRED ###############
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

one_hot_encoding <- function(data, column_names) {
  for (column_name in column_names) {
    # Get unique values in the specified column
    unique_values <- unique(data[[column_name]])
    # Create new columns for each unique value and populate with binary values
    for (value in unique_values) {
      value = gsub(" ","_",value)
      value = gsub("/","_",value)
      value = gsub("-","_",value)
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

fittedprediction <- reactive({
  completerow <- laty %>%
    filter(postal == input$address)
  
  if(nrow(completerow) > 0) {
    return(completerow)
  } else {
    return(data.frame(Street = "Street name not found"))
  }
})

generate_month_dummies <- function() {
  # Get the current year and month
  current_year <- 2017
  current_month <- 1
  
  # Initialize a data frame to store the dummy variables
  time <- data.frame(year = numeric(), month = character())
  year = c()
  month = c()
  months = (2024 - 2017) * 12 + 3 # No of months from Jan 2017 to Apr 2024
  # Iterate over the number of months
  for (i in 0:months) {
    # Calculate the year and month for the current iteration
    next_year <- current_year + floor((current_month + i - 1) / 12)
    next_month <- ((current_month + i - 1) %% 12) + 1
    time = rbind(time, c(next_year,next_month))
  }
  names(time)[1] = "year"
  names(time)[2] = "month"
  return (time)
}
