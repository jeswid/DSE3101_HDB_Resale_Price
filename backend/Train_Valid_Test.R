# Set seed for reproducibility
set.seed(42)

hdb_data = read.csv("backend/processed_data/hdb_merged_no_transform.csv") %>% 
  select(-1) %>%
  mutate(month = as.character(month),postal_2dig = as.character(postal_2dig))

# Stratified Sampling by 'year'
strata_cols <- c("year")

# Perform the stratified sampling for training set (60%)
train_data <- hdb_data %>%
  group_by(across(all_of(strata_cols))) %>%
  sample_frac(0.6)  # Adjust the fraction as needed for training set

# Remaining data for validation and test sets
remaining_data <- anti_join(hdb_data, train_data)

# Split remaining data into validation and test sets (20% each)
validation_data <- remaining_data %>%
  group_by(across(all_of(strata_cols))) %>%
  sample_frac(0.5)  # Adjust the fraction as needed for validation set

test_data <- anti_join(remaining_data, validation_data)

# Reset row names
rownames(train_data) <- NULL
rownames(validation_data) <- NULL
rownames(test_data) <- NULL

get_categorical_columns <- function(data) {
  categorical_columns <- character(0)  # Initialize an empty character vector to store column names
  for (col in names(data)) {
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  return(categorical_columns)
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

cat_columns <- hdb_data %>% get_categorical_columns()
train_data = one_hot_encoding(train_data, cat_columns)
validation_data = one_hot_encoding(validation_data, cat_columns)
test_data = one_hot_encoding(test_data, cat_columns)