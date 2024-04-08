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
  
  ############## MAP TAB ##################################################################
  
  output$geoSelectionOutput <- renderText({
    paste("You have selected:", "BLK", as.character(text_data()$street))
  })
  
  data <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      mutate(INFO = paste(street))
  })
  
  text_data <- reactive({
    all_address %>%
      filter(postal %in% input$addressM)
  })
  

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
      addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~INFO, label = ~INFO) 
  })
  
  data_table_mrt <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`mrt_name`, `dist_to_nearest_mrt`, `mrt_1km`) %>%
      rename("Nearest MRT Station" = `mrt_name`, 
             "Distance to nearest mrt (in km)" = `dist_to_nearest_mrt`,
             "MRT stations within 1km" = `mrt_1km`)
  })
  
  output$mrt_table <- renderTable({data_table_mrt()})
  
  data_table_sch <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`primary_school_name`, `dist_to_nearest_primary_schools`, `primary_schools_1km`) %>%
      rename("Nearest Primary School" = `primary_school_name`,
             "Distance to nearest primary school (in km)" = `dist_to_nearest_primary_schools`, 
             "Primary Schools within 1km" = `primary_schools_1km`)
  })
  
  output$sch_table <- renderTable({data_table_sch()})
  
  data_table_supermarket <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`nearest_supermarket`, `dist_to_nearest_supermarket`, `supermarket_1km`) %>%
      rename("Nearest Supermarket" = `nearest_supermarket`,
             "Distance to nearest supermarket (in km)" = `dist_to_nearest_supermarket`, 
             "Supermarkets within 1km" = `supermarket_1km`)
  })
  
  output$supermarket_table <- renderTable({data_table_supermarket()})
  
  data_table_hawker <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`nearest_hawkers`, `dist_to_nearest_hawkers`, `hawkers_1km`) %>%
      rename("Nearest Hawker Centre" = `nearest_hawkers`,
             "Distance to nearest Hawker Centre (in km)" = `dist_to_nearest_hawkers`, 
             "Hawker Centres within 1km" = `hawkers_1km`)
  })
  
  output$hawkers_table <- renderTable({data_table_hawker()})

  ########################## HOME TAB ######################################################
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
  

  ######################## FUNCTIONS NEEDED ########################################################
  street_name <- reactive({
    matched_row <- laty[laty$postal == input$address, ]
    
    if(nrow(matched_row) > 0) {
      return(matched_row$street)
    } else {
      return("Street name not found")
    }
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
  fittedforecast <- reactive({
    completerow <- laty %>%
      filter(postal == input$addressF)
    
    if(nrow(completerow) > 0) {
      return(completerow)
    } else {
      return(data.frame(Street = "Street name not found"))
    }
  })
 
  
###################PREDICTED PRICE TAB #############################################################  
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
      output$priceOutput <- renderText({ selection_message })
      
      # Show the predicted price in a modal dialog
      if(nrow(filtered_row) > 0) {
        showModal(modalDialog(
          title = "Predicted Price",
          h3(paste("$", formatC(prediction, format = "f", big.mark = ",", digits = 0), sep="")),
          footer = modalButton("Close")
        ))
      } else {
        # You can also use a modal to show an error message if you prefer
        showModal(modalDialog(
          title = "Error",
          "Please ensure a valid postal code is selected.",
          footer = modalButton("Close")
        ))
      } }
  })
  ##### FORECASTED PRICE TAB ###################################################################################
  observeEvent(input$submitforecast, {
    req(input$addressF)
    filtered_row <- fittedforecast()  # Fetch the filtered dataset based on postal code

    if(nrow(filtered_row) > 0) {
      # Prepare geospatial data and additional user inputs for prediction
      geospatial_data <- filtered_row %>%
        select(-c(postal, street))
      month_dummies = generate_month_dummies(input$no_of_years_forecast) # Let user input number of years to forecast
      forecasted_prices = data.frame()

      for (i in 1:nrow(month_dummies)) {
        current_year = month_dummies$year[i]
        current_month = month_dummies$month[i]
        current_month <- ifelse(current_month < 10, paste0("0", as.character(current_month)), as.character(current_month))

        final_row <- geospatial_data %>%
          mutate(ave_storey = input$storeyF,
                 flat_model = input$flat_modelMF,
                 flat_type = input$flat_typeF,
                 floor_area_sqm = input$floor_area_sqmF,
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
        forecast = data.frame(months_ahead = i - 1, forecasted_price = prediction)
        forecasted_prices = rbind(forecasted_prices,forecast)
      }


      # Construct user selection message
      selection_message <- paste("Your choice:", street_name(), ",", input$flat_typeF,
                                 input$flat_modelMF, "FLAT AT LEVEL", input$storeyF, sep = "\n")

      # Combine selection message with prediction
      output$priceOutputF <- renderText({ selection_message })

      output$forecastChart <- renderPlotly({
        req(input$submitforecast)  # Require that the forecast button has been clicked

        # Generate the line chart
        graph = ggplot(forecasted_prices, aes(x = months_ahead, y = forecasted_price)) +
          geom_line() +
          labs(title = "Forecasted HDB Prices", x = "Number of months ahead", y = "Price of HDB flat") +
          scale_x_continuous(breaks = seq(0, input$no_of_years_forecast * 12, by = 4)) +
          theme_minimal()
        ggplotly(graph)
      })

      }
  })
    
})  
  
    
 