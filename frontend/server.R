library('leaflet')
library('htmltools')
library('DT')
library('dplyr')
library('tidyr')
library('ggplot2')
library("htmltools")
library('shiny')
library('shinydashboard')
library('leaflet')
library('RColorBrewer')

shinyServer(function(input, output, session) {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  data <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      mutate(INFO = paste(sep = "<br/>",
                          town, "\n" , 
                          "Nearest mrt:", `mrt_name`, "\n", 
                          "Nearest primary school:", `primary_school_name`))
  })
  
  # text_data <- reactive({
  #   all_address %>%
  #     filter(postal %in% input$addressM)
  # })
  # 
  # output$geoSelectionOutput <- renderText({
  #   as.character(text_data()$street)
  # })
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = "red"
  )
  
  output$map <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5) %>%
      addTiles() %>%
      addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~INFO, label = ~INFO) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  

  observeEvent(input$submitprice, {
    output$priceOutput <- renderText({
      if(!is.null(input$address) && street_name() != "Street name not found") {
        paste("Your choice:",
              street_name(), ",", input$flat_type,
              input$flat_modelM, "FLAT AT LEVEL", input$storey, sep = "\n")
      } else {
        "Please ensure a valid postal code is selected."
      }
    })
  })

  street_name <- reactive({
    matched_row <- laty[laty$postal == input$address, ]
    
    if(nrow(matched_row) > 0) {
      return(matched_row$street)
    } else {
      return("Street name not found")
    }
  })
  

  

  
  
  output$homeOutput <- renderUI({
    HTML(paste0('
    <div style="font-size: 15px; line-height: 1.6;">
      <h2>Introduction</h2>
      <p>First-time home buyers frequently face challenges such as planning their budget, selecting a suitable location, and understanding the dynamics of the property market.</p>
      <p>Traditionally, house prices have been thought to reflect their proximity to urban centers, transportation networks, and amenities. Yet, the real connection between a property\'s practical benefits and its market value can be obscured by casual discussions or the perspectives of sales agents.</p>
      <p>Our aim is to resolve these issues by providing buyers with crucial knowledge, enabling them to make informed decisions when purchasing their ideal home. This, in turn, reduces their reliance on external intermediaries like property agents.</p>
      <h2>Data Sources</h2>
      <p>Our data are meticulously curated from a variety of sources to ensure a balanced and precise experience for users of our website:</p>
      <ul>
        <li>HDB Resale Prices and Transaction History: Acquired from the authoritative database at data.gov.sg, offering insight into historical trends and current market values.</li>
        <li>Amenities, Transportation Links, and Proximity to the Central Business District (CBD): Sourced from OneMap.sg, this data provides comprehensive details on local infrastructure, enhancing our understanding of property desirability and accessibility.</li>
        <li>Demographic and Household Data: Compiled from the Singapore Department of Statistics, offering a detailed overview of demographic shifts and household compositions, pivotal for informed decision-making in the property market.</li>
      </ul>
    </div>'
    ))
  })
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
    filtered_row <- fittedprediction()  # Fetch the filtered dataset based on postal code
    
    if(nrow(filtered_row) > 0) {
      # Prepare geospatial data and additional user inputs for prediction
      geospatial_data <- filtered_row %>%
        select(-c(postal, street))
      current_date <- Sys.Date()
      current_year <- as.numeric(format(current_date, "%Y"))
      current_month <- as.numeric(format(current_date, "%m"))
      current_month <- ifelse(current_month < 10, paste0("0", as.character(current_month)), as.character(current_month))
      
      final_row <- geospatial_data %>%
        mutate(ave_storey = input$storey,
               flat_model = input$flat_modelM,
               flat_type = input$flat_type,
               floor_area_sqm = input$floor_area_sqm,
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
      
      # Construct user selection message
      selection_message <- paste("Your choice:", street_name(), ",", input$flat_type,
                                 input$flat_modelM, "FLAT AT LEVEL", input$storey, sep = "\n")
      
      # Combine selection message with prediction
      final_message <- paste(selection_message, "Predicted price:", prediction, sep = "\n\n")
      
      # Display the combined message
      output$priceOutput <- renderText({ final_message })
      
    } else {
      # Handle case where no valid data is found for the postal code
      output$priceOutput <- renderText({ "Please ensure a valid postal code is selected." })
    }
  })
  
  
  # observeEvent(input$forecastprice, {
  #   req(input$address)
  #   filtered_row <- fittedprediction()  # Fetch the filtered dataset based on postal code
  #   
  #   if(nrow(filtered_row) > 0) {
  #     # Prepare geospatial data and additional user inputs for prediction
  #     geospatial_data <- filtered_row %>%
  #       select(-c(postal, street))
  #     current_date <- Sys.Date()
  #     current_year <- as.numeric(format(current_date, "%Y"))
  #     current_month <- as.numeric(format(current_date, "%m"))
  #     current_month <- ifelse(current_month < 10, paste0("0", as.character(current_month)), as.character(current_month))
  #     
  #     final_row <- geospatial_data %>%
  #       mutate(ave_storey = input$storey,
  #              flat_model = input$flat_modelM,
  #              flat_type = input$flat_type,
  #              floor_area_sqm = input$floor_area_sqm,
  #              year = current_year, 
  #              month = current_month, 
  #              remaining_lease = 99 - (current_year - lease_commence_date + as.numeric(current_month) / 12)) %>%
  #       select(-lease_commence_date)
  #     
  #     # Data transformation and encoding for ML model input
  #     sample_obs_before_encoding <- convert_to_categorical(final_row, get_categorical_columns(final_row))
  #     sample_obs_transformed <- one_hot_encoding(sample_obs_before_encoding, get_categorical_columns(sample_obs_before_encoding))
  #     newdata <- rbind.fill(sample_df, sample_obs_transformed)
  #     newdata[is.na(newdata)] <- 0
  #     newdata <- newdata[2,]
  #     newdata <- as.matrix(newdata)
  #     
  #     # ML model prediction
  #     prediction <- exp(predict(model, newdata))
  #     
  #     # Construct user selection message
  #     selection_message <- paste("Your choice:", street_name(), ",", input$flat_type,
  #                                input$flat_modelM, "FLAT AT LEVEL", input$storey, sep = "\n")
  #     
  #     # Combine selection message with prediction
  #     final_message <- paste(selection_message, "Forecasted price:", prediction, sep = "\n\n")
  #     
  #     # Display the combined message
  #     output$forecastOutput <- renderText({ final_message })
  #     
  #   } else {
  #     # Handle case where no valid data is found for the postal code
  #     output$forecastOutput <- renderText({ "Please ensure a valid postal code is selected." })
  #   }
  # })
  
  
  
  
  
  
})