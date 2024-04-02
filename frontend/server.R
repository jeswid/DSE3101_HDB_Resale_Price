library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)

shinyServer(function(input, output, session) {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
  # Assuming all_address is a data frame with your data
  # Example for all_address
  # all_address <- data.frame(lat = runif(10, min = 1.2, max = 1.5),
  #                           long = runif(10, min = 103.6, max = 104),
  #                           town = LETTERS[1:10])
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    all_address %>%
      filter(mrt == all_address$mrt_1km, 
             hawkers == alladress$hawkers_1km, 
             schs == alladdress$primary_schools_1km, 
             hospitals == alladress$hospitals_1km,
             town == alladdress$street
      )
  })
  
  # Color palette reactive (example logic)
  colorpal <- reactive({
    colorNumeric("RdYlBu", domain = filteredData()$some_value)
  })
  
  # Initial map creation without markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5) %>%
      setMaxBounds(lng1 = 103.600250, lat1 = 1.202674, lng2 = 104.027344, lat2 = 1.484121)
  })
  
  findRegion <- function(lat, lng) {
    distances <- sqrt((all_address$lat - lat)^2 + (all_address$lng - lng)^2)
    closest_index <- which.min(distances)
    closest_region <- all_address[closest_index, ]
    return(closest_region)
  }
  
  observe({
    click <- input$map_click
    if(is.null(click)) return()
    
    clicked_region <- findRegion(click$lat, click$lng)
    if(is.null(clicked_region)) return()
    
    content <- as.character(tagList(
      tags$h4("Town of selected house:", clicked_region$town),
      sprintf("Mrt within 1km: %s%", as.integer(clicked_region$mrt_1km)),
      sprintf("Primary schools within 1km: %s%", as.integer(clicked_region$primary_schools_1km)), 
      sprintf("Hawker centres within 1km: %s", as.integer(clicked_region$hawkers_1km))
    ))
    
    leafletProxy("map") %>%
      clearPopups() %>%  # Clear existing popups
      addPopups(lng = click$lng, lat = click$lat, content)
  })
  
  observeEvent(input$submitgeo, {
    # Assuming you want to display some details based on geospatial analysis inputs
    output$geoSelectionOutput <- renderText({
      paste("Geospatial Selection:", input$addressM, input$townM, input$flat_modelM, input$flat_typeM, input$amenitiesM, sep = "\n")
    })
  })
  
  observeEvent(input$submitprice, {
    # Assuming you want to display some details based on predicted price inputs
    output$priceOutput <- renderText({
      paste("Predicted Price Selection:", input$address, input$town, input$flat_model, input$flat_type, input$amenities, sep = "\n")
    })
  })
  
})
  
  
  
  
  
  
  
  
  
  
  
  
  # output$geoOutput <- renderText({ "Map will be displayed here." })
  # output$priceOutput <- renderText({ "Predicted price will be shown here." })
  # 
  # observeEvent(input$submit, {
  #   # This is where you can process the inputs and update the outputs.
  #   # For example:
  #   output$homeOutput <- renderText({
  #     #make the chosen into a box instead of a line
  #     paste("You have selected:", input$address, input$town, input$flat_model)
  #   })
  # })
  # output$geoOutput <- renderText({ "Map will be displayed here." })
  # 
  # observeEvent(input$submit, {
  #   # Construct the text with the selected inputs
  #   output$priceOutput <- renderText({
  #     paste("You have selected:", 
  #           input$address,input$town,input$flat_model,input$flat_type, input$amenities, sep = "\n") })
  #   
  #   
  #    # Separate items by a new line
  # })
  # 
  # })
  # 
  # 
  # 
  # 
