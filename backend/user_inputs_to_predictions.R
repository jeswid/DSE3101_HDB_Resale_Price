# Converting the user inputs into model output
library(plyr)
library(tidyverse)
library(plotly)

# Fit the obs with 136 variables into the ML model (Does not include resale prices)
# Below is a sample observation
sample_obs = readRDS("backend/processed_data/hdb_resale_prices.Rds")[1,] %>%
  select(-resale_price)

# Use this to complement the user inputs
hdb = readRDS("backend/processed_data/lat_long_for_prediction.Rds") 
hdb = hdb[1,]

sample_obs_before_encoding = readRDS("backend/processed_data/hdb_merged_no_transform.Rds")[1,] %>% 
  select(-resale_price) %>%
  mutate(month = as.character(month)) # Convert month to character to utilise get_categorical_columns

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

sample_obs_before_encoding <- convert_to_categorical(sample_obs_before_encoding,
                                                     get_categorical_columns(sample_obs_before_encoding))

sample_obs_transformed <- one_hot_encoding(sample_obs_before_encoding, 
                                 get_categorical_columns(sample_obs_before_encoding))

df = readRDS("backend/processed_data/hdb_resale_prices.Rds")[1,] %>% 
  select(-resale_price)
hdb = readRDS("backend/processed_data/lat_long_for_prediction.Rds") 
hdb = hdb[1,]

a = hdb %>% select(-c(postal,street)) %>%
  mutate(ave_storey = 5,
         flat_model = "2-room",
         flat_type = "1 ROOM",
         floor_area_sqm = 44,
         year = 2024, 
         month = "03", 
         remaining_lease = 99 - as.numeric(2024 - lease_commence_date - as.numeric("3")/12)) %>%
  select(-lease_commence_date)
a <- convert_to_categorical(a,get_categorical_columns(a))

a <- one_hot_encoding(a, get_categorical_columns(a))
df = readRDS("backend/processed_data/hdb_resale_prices.Rds")[1,] %>% 
  select(-resale_price)

# saveRDS(df, "backend/processed_data/sample_obs_inputs.Rds")
newdata = rbind.fill(df, a)
newdata[is.na(newdata)] <- 0
newdata = newdata[2,]

xgb.fit = readRDS("backend/xgb.rds")
newdata = as.matrix(newdata)
prediction <- exp(predict(xgb.fit, newdata))
prediction

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

# Example usage:
month_dummies <- generate_month_dummies()
print(month_dummies)

model = readRDS("backend/xgb.rds")
sample_df = readRDS("frontend/data/sample_obs_inputs.Rds")
geospatial_data <- hdb %>% select(-c(postal,street))
month_dummies = generate_month_dummies() 
forecasted_prices = data.frame()

for (i in 1:nrow(month_dummies)) {
  current_year = month_dummies$year[i]
  current_month = month_dummies$month[i]
  current_month <- ifelse(current_month < 10, paste0("0", as.character(current_month)), as.character(current_month))
  
  final_row <- geospatial_data %>%
    mutate(ave_storey = 5,
           flat_model = "2-room",
           flat_type = "1 ROOM",
           floor_area_sqm = 44,
           year = current_year,
           month = current_month,
           remaining_lease = 99 - (current_year - lease_commence_date + as.numeric(current_month) / 12)) %>%
    select(-lease_commence_date)
  
  # Data transformation and encoding for ML model input
  sample_obs_before_encoding <- convert_to_categorical(final_row, get_categorical_columns(final_row))
  sample_obs_transformed <- one_hot_encoding(sample_obs_before_encoding, get_categorical_columns(sample_obs_before_encoding))
  newdata <- rbind.fill(sample_df, sample_obs_transformed)
  newdata[is.na(newdata)] <- 0
  newdata <- newdata[2,]
  newdata <- as.matrix(newdata)
  # ML model prediction
  prediction <- exp(predict(model, newdata))
  forecast = data.frame(months_ahead = 2017 + (i - 1)/12, forecasted_price = prediction)
  forecasted_prices = rbind(forecasted_prices,forecast)
}
graph = ggplot(forecasted_prices, aes(x = months_ahead, y = forecasted_price)) +
  geom_line() +
  labs(title = "Forecasted HDB Prices", x = "Year", y = "Price of HDB flat") +
  theme_minimal()
ggplotly(graph)
