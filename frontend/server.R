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
      filter(postal %in% input$addressM, 
      ) %>%
      mutate(INFO = paste0(town, " | ", lng, ", ", lat))
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
      addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~INFO, label = ~INFO) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
  observeEvent(input$submitprice, {
    output$priceOutput <- renderText({
      paste("Your choice:", 
            street_name(),",",input$flat_type, 
            input$flat_modelM,"FLAT AT LEVEL", input$storey, sep = "\n")
    })})
    
  street_name <- reactive({
    # Find the row in the data where the postal code matches the input
    matched_row <- all_address[all_address$postal == input$addressM, ]
    
    # If there's a match, return the street name; otherwise, return NA or an informative message
    if(nrow(matched_row) > 0) {
      return(matched_row$street)
    } else {
      return(NA) # Or return something like "Street name not found"
    }
  })
  
  # Output the street name
  output$geoSelectionOutput <- renderText({
    if(!is.null(input$addressM) && !is.na(street_name())) {
      paste("You have selected:", street_name(), sep = "\n")
    } else {
      "Please select a valid postal code."
    }
  })
  
  
})