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
library('lubridate')
library('anytime')
library('knitr')
library('kableExtra')
library('stats')

source("global_settings.R")
shinyServer(function(input, output, session) {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  ############## MAP TAB ##################################################################
  output$intro2 <- renderText({paste("How to use: Type or Select HDB postal code. 
                                     Or if you just want to explore the HDB blocks in a town, 
                                     feel free to select the desired town to check out the HDBs 
                                     available too!")})
  
  # Create a reactive expression to filter the choices of postal codes based on the selected town
  filtered_postal_codes <- reactive({
    if (!is.null(input$town)) {
      if(input$town == 'ALL TOWNS') {
        all_address %>% 
          arrange(desc(postal)) %>%
          pull(postal) %>%
          unique()
      } else {
        all_address %>%
          filter(town == input$town) %>%
          arrange(desc(postal)) %>%
          pull(postal) %>%
          unique()
      }} else {
        NULL
      }
  })
  
  output$geoSelectionOutput <- renderText({
    paste("You have selected:", "BLK", as.character(text_data()$street))
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
  
  
  data <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      mutate(INFO = paste(street))
  })
  
  output$map <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5) %>%
      addTiles() %>%
      addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~INFO, label = ~INFO)
  })
  
  # Update the choices of postal codes based on the selected town
  observe({
    updateSelectInput(session, "addressM", choices = filtered_postal_codes())
  })
  
  observeEvent(input$submitmap, {
    req(input$addressM)
    data = data()
    click <- input$submitmap
    if (!is.null(click)) {
      lng <- data$lng
      lat <- data$lat
      
      # Update map view to zoom into the clicked location
      leafletProxy("map") %>%
        setView(lng = lng, lat = lat, zoom = 18)
    }
  })
  
  data_table_mrt <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`mrt_name`, `dist_to_nearest_mrt`, `mrt_1km`) %>%
      mutate(`dist_to_nearest_mrt` = round(`dist_to_nearest_mrt`,2)) %>%
      rename("Nearest MRT Station" = `mrt_name`,
             "Distance to nearest MRT (in km)" = `dist_to_nearest_mrt`,
             "MRT stations within 1km" = `mrt_1km`)
  })
  
  output$mrt_table <- renderDT({
    datatable(data_table_mrt(),
              rownames = FALSE,
              options = list(
                lengthMenu = FALSE,
                searching = FALSE,
                paging = FALSE
              )
    )
  })
  
  data_table_sch <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`primary_school_name`, `dist_to_nearest_primary_schools`, `primary_schools_1km`) %>%
      mutate(`dist_to_nearest_primary_schools` = round(`dist_to_nearest_primary_schools`,2)) %>%
      rename("Nearest Primary School" = `primary_school_name`,
             "Distance to nearest Primary School (in km)" = `dist_to_nearest_primary_schools`,
             "Primary Schools within 1km" = `primary_schools_1km`)
  })
  
  output$sch_table <- renderDT({
    datatable(data_table_sch(),
              rownames = FALSE,
              options = list(
                lengthMenu = FALSE,
                searching = FALSE,
                paging = FALSE
              )
    )
  })
  
  
  data_table_hawker <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`name_of_centre`, `dist_to_nearest_hawkers`, `hawkers_1km`) %>%
      mutate(`dist_to_nearest_hawkers` = round(`dist_to_nearest_hawkers`,2)) %>%
      rename("Nearest Hawker Centre" = `name_of_centre`,
             "Distance to nearest Hawker Centre (in km)" = `dist_to_nearest_hawkers`,
             "Hawker Centres within 1km" = `hawkers_1km`)
  })
  
  output$hawkers_table <- renderDT({
    datatable(data_table_hawker(),
              rownames = FALSE,
              options = list(
                lengthMenu = FALSE,
                searching = FALSE,
                paging = FALSE
              ))
  })
  
  data_table_hospital <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`nearest_hospital`, `dist_to_nearest_hospital`, `hospitals_1km`) %>%
      mutate(`dist_to_nearest_hospital` = round(`dist_to_nearest_hospital`,2)) %>%
      rename("Nearest Hospital" = `nearest_hospital`,
             "Distance to nearest Hospital (in km)" = `dist_to_nearest_hospital`,
             "Hospitals within 1km" = `hospitals_1km`)
  })
  
  output$hospitals_table <- renderDT({
    datatable(data_table_hospital(),
              rownames = FALSE,
              options = list(
                lengthMenu = FALSE,
                searching = FALSE,
                paging = FALSE
              )
    )
  })
  
  data_table_supermarket <- reactive({
    all_address %>%
      filter(postal %in% input$addressM) %>%
      select(`nearest_supermarket`, `dist_to_nearest_supermarket`, `supermarket_1km`) %>%
      mutate(`dist_to_nearest_supermarket` = round(`dist_to_nearest_supermarket`,2)) %>%
      rename("Nearest Supermarket" = `nearest_supermarket`,
             "Distance to nearest Supermarket (in km)" = `dist_to_nearest_supermarket`,
             "Supermarkets within 1km" = `supermarket_1km`)
  })
  
  output$supermarket_table <- renderDT({
    datatable(data_table_supermarket(),
              rownames = FALSE, 
              options = list(
                lengthMenu = FALSE,
                searching = FALSE,
                paging = FALSE
              )
              
              
    )
  })
  
  
  ########################## HOME TAB ######################################################
  output$homeOutput <- renderUI({
    tags$img(src = "hdb_2.jpg", style = "width:100%;")
    HTML(paste0('
  <div style="font-size: 15px; line-height: 1.6;">
    <h2>Introduction</h2>
    <p>First-time home buyers frequently face challenges such as planning their budget, selecting a suitable location, and understanding the dynamics of the property market.</p>
    <p>Our website will provide users a map of Singapore to search for their desired HDB block and view curated information about the nearest amenities to their HDB block. Buyers can select the desired characteristics of their HDB resale flat and a predicted price of the HDB resale flat will be churned out. Finally, they can view trends in prices of that HDB resale flat from 2017 - 2024 via a graph. The prediction of prices and plotting of the graph is powered by our Extreme Gradient Boosting Machine Learning Model, which we have found to perform better than our benchmark ordinary least squares regression.</p>
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
      
      
      # Show the predicted price in a modal dialog
      if(nrow(filtered_row) > 0) {
        showModal(modalDialog(
          title ="The predicted price of the HDB resale price at:",
          h4(paste(paste("BLK", street_name()) , input$flat_type, input$flat_modelM, 
                   "FLAT at LEVEL", input$storey), sep = "\n"),
          h4(paste("REMAINING LEASE:",round(final_row$remaining_lease,2),"years"), sep = "\n"),
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
  
  output$intro <- renderText({paste("How to use: Use the sliders to key in the desired house size and storeys, 
                                    as well as key in the HDB postal code, flat model and flat type to see 
                                    the predicted price of your chosen HDB unit")})
  ##### FORECASTED PRICE TAB ###################################################################################
  output$intro1 <- renderText({paste("How to use: Use the sliders to key in the desired house size and storeys, 
                                    as well as key in the HDB postal code, flat model and flat type to see 
                                    the trend in the price of your chosen HDB unit. Hover your mouse above the specific points
                                     to see the exact prices and growth rates at specific dates. Zoom in on the graph to observe specific
                                     details in the HDB price trends and growth rates.")})
  
  observeEvent(input$submitforecast, {
    req(input$addressF)
    filtered_row <- fittedforecast()  # Fetch the filtered dataset based on postal code
    
    if(nrow(filtered_row) > 0) {
      # Prepare geospatial data and additional user inputs for prediction
      geospatial_data <- filtered_row %>%
        select(-c(postal, street))
      month_dummies = generate_month_dummies() # Let user input number of years to forecast
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
        forecast = data.frame(year = 2017 + (i)/12, predicted_price = prediction)
        forecasted_prices = rbind(forecasted_prices,forecast)
        if (i == nrow(month_dummies)) {
          lease_remained = final_row$remaining_lease
        }
      }
      
      
      
      
      # Construct user selection message
      selection_message <- paste("HDB Price Trend:", paste("BLK", street_name()), input$flat_typeF,
                                 input$flat_modelMF, paste("FLAT AT LEVEL", input$storeyF), 
                                 "with remaining lease at present of", 
                                 round(lease_remained,2),"years")
      
      # Combine selection message with prediction
      output$priceOutputF <- renderText({ selection_message })
      
      output$forecastChart <- renderPlotly({
        req(input$submitforecast)  # Require that the forecast button has been clicked
        
        # Convert decimal years to dates
        forecasted_prices$date <- my(paste(ifelse(round((forecasted_prices$year - floor(forecasted_prices$year)) * 12) == 0, 
                                                  12, round((forecasted_prices$year - floor(forecasted_prices$year)) * 12)), 
                                           ifelse(round((forecasted_prices$year - floor(forecasted_prices$year)) * 12) == 0, 
                                                  (floor(forecasted_prices$year)-1), floor(forecasted_prices$year)),sep = "-"))
        
        
        
        # Sort the dataset by date if it's not already sorted
        forecasted_prices <- forecasted_prices[order(forecasted_prices$date), ]
        
        years_req <- c(sum(2017+(1/12)), sum(2018+(1/12)), sum(2019+(1/12)), sum(2020+(1/12)), sum(2021+(1/12)), 
                       sum(2022+(1/12)), sum(2023+(1/12)), sum(2024+(1/12)))
        
        calculate_year_on_year_growth <- function(years, prices) {
          growth_rates <- numeric(length(years))
          
          for (i in 1:length(years)) {
            if (years[i] == sum(2017+(1/12))) {
              growth_rates[i] <- 0
            } else {
              growth_rates[i] <- ((prices[i] - prices[i-1]) / prices[i-1]) * 100
            }
          }
          
          return(growth_rates)
        }
        
        
        final_prices <- forecasted_prices %>% filter(year %in% years_req) %>% pull(predicted_price)
        
        # Calculate the year-on-year growth rate for each year
        growth_rates <- calculate_year_on_year_growth(years_req, final_prices)
        
        growth_df <- data.frame ( year = years_req, price = final_prices)
        
        # Calculate the average growth rate
        avg_growth <- mean(growth_rates, na.rm = TRUE)
        
        #for trend line
        m <- loess(predicted_price~as.numeric(date),data = forecasted_prices)
        
        # Generate the line chart
        # Create plotly graph with a trendline on the price changes over the years, label by average growth rate
        p <- plot_ly(data = forecasted_prices, x = ~date, y = ~predicted_price, type = 'scatter', mode = 'lines+markers',
                     text = ~paste("Date: ", date, paste0("<br>", "Price: $", round(predicted_price, 2))),
                     hoverinfo = 'text', name = "Monthly Price") %>%
          layout(title = "Trend of HDB Prices",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Price of HDB", tickformat = "$")) %>%
          add_lines(x=~date, y=predict(m), name="General Trend", hoverinfo = 'none') %>%
          add_annotations(x = '2023-01-01', y = 650000, text = paste0("Average 
                           Year-On-Year Price Growth Rate: ", round(avg_growth, 2), "%"), showarrow = FALSE)
        
        q <- plot_ly(data = growth_df, x = floor(years_req), y = growth_rates/100, type = 'bar', name = "Growth Rate", marker = list(color = ifelse(growth_rates < 0, "pink", "lightgreen"))) %>%
          layout(title = "HDB Price Trends & Annual Growth Rates",
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Growth Rate", tickformat = "%"))
        
        
        final <- subplot(p, q, nrows = 2)
        
        final
        
      })
      
    }
  })
  
})  


