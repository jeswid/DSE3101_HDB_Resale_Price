# Converting the user inputs into model output
library(plyr)

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
newdata = rbind.fill(sample_obs_transformed, df)

# prediction <- exp(predict(regfit, newdata = your_line_of_data))