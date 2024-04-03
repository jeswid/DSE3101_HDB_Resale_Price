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
  
  filtered_data <- reactive({
    if (!is.null(input$address) && !is.null(input$amenities)) {
      print("Selected Postal Code:")
      print(input$address)
      
      print("Unique Postal Codes in Dataset:")
      print(unique(all_address$postal))
      
      selected_data <- all_address %>%
        filter(postal == input$address) %>%
        select(lat, lng, town, amenities = input$amenities) %>%
        unique() %>%
        slice(1)
      
      print("Filtered Data:")
      print(selected_data)
      
      return(selected_data)
    } else {
      return(NULL)
    }
  })
  
  
  # Initial map creation without markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5) %>%
      setMaxBounds(lng1 = 103.600250, lat1 = 1.202674, lng2 = 104.027344, lat2 = 1.484121)
  })
  
  observeEvent(input$submitmap, {
    selected_data <- filtered_data()
    
    if (!is.null(selected_data) && nrow(selected_data) > 0) {
      leafletProxy("map") %>% 
        clearMarkers() %>%
        addCircleMarkers(lng = selected_data$lng, 
                         lat = selected_data$lat, 
                         popup = ~paste("Town: ", selected_data$town, 
                                        "<br/>", "Amenities: ", selected_data$amenities))
    } else {
      leafletProxy("map") %>%
        clearMarkers()  # Clear markers from the map
      print("No data found for the provided address.")
    }
  })
  
  observeEvent(input$submitprice, {
    output$priceOutput <- renderText({
      paste("Predicted Price Selection:", 
            input$address, input$town, input$flat_model, 
            input$flat_type, input$amenities, sep = "\n")
    })
  })
})
