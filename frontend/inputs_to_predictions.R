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

observeEvent(input$submitprice, {
  req(input$address)
  filtered_row <- fittedprediction() # Make sure this is defined as before
  
  if(nrow(filtered_row) > 0) {
    geospatial_data = filtered_row %>% # 14 variables
      select(-c(postal,street))
    current_date <- Sys.Date()
    current_year <- as.numeric(format(current_date, "%Y"))
    current_month <- as.numeric(format(current_date, "%m"))
    if (current_month < 10) {
      current_month = paste0("0",as.character(current_month))
    }
    else{
      current_month = as.character(current_month)
    }
    final_row <- geospatial_data %>% # include the 6 user inputs (Convert lease commence to remaining lease)
      mutate(ave_storey = input$storey,
             flat_model = input$flat_modelM,
             flat_type = input$flat_type,
             floor_area_sqm = input$floor_area_sqm,
             year = current_year, 
             month = current_month, 
             remaining_lease = 99 - as.numeric(current_year - 
                                                 lease_commence_date - as.numeric(current_month)/12)) %>%
      select(-lease_commence_date)
    sample_obs_before_encoding <- convert_to_categorical(final_row,
                                                         get_categorical_columns(final_row))
    
    sample_obs_transformed <- one_hot_encoding(sample_obs_before_encoding, 
                                               get_categorical_columns(sample_obs_before_encoding))
    newdata = rbind.fill(sample_df, sample_obs_transformed)
    newdata[is.na(newdata)] <- 0
    newdata = newdata[2,]
    newdata = as.matrix(newdata)
    prediction <- exp(predict(model, newdata))
    
    # Display the predicted price as a text
    output$predictedPriceOutput <- renderText({
      paste("Predicted price:", prediction)
    })
    
  } else {
    # Handle the case where no data is found
    output$predictedPriceOutput <- renderText({
      "No data found for the provided postal code."
    })
  }
})

