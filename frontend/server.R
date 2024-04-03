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
    markerColor = "blue"
  )
  
  output$map <- renderLeaflet({
    leaflet(data = data()) %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5) %>%
      addTiles() %>%
      addAwesomeMarkers(~lng, ~lat, icon = icons, popup = ~INFO, label = ~INFO) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
  })
  
})