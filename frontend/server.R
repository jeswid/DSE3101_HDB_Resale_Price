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



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  ######################## REACTIVE FUNCTION ###########################################################
  filteredData <- reactive({
    all_address %>%
      filter(mrt == all_address$`mrt_1km`, 
             hawkers == alladress$`hawkers_1km`, 
             schs == alladdress$`primary_schools_1km`, 
             hospitals == alladress$`hospitals_1km`,
             town == alladdress$street
             )
    })
  
  colorpal <- reactive({
  colorNumeric(input$colors, median_resale_prices$median_price)
  })
  
  filteredhawkers <- reactive({
    hawker_centres %>% 
      filter(., Year == input$yr, ENAME == input$dist_id)
  })
  
  filteredhosp <- reactive({
    hospitals %>% 
      filter(., Year == input$yr, ENAME == input$dist_id)
  })
  

  ################################## M A P   L A Y E R ########################################################
  # Create the map
  output$map = renderLeaflet({
    leaflet() %>% 
      addTiles () %>%
      setView(lng = 103.8198, lat = 1.28, zoom = 10.5)%>%
      setMaxBounds( lng1 = 103.600250,
                    lat1 = 1.202674,
                    lng2 = 104.027344,
                    lat2 = 1.484121 ) 
  })
  
  

  
  observeEvent(input$submit, {
    # This is where you can process the inputs and update the outputs.
    # For example:
    output$homeOutput <- renderText({
      #make the chosen into a box instead of a line
      paste("You have selected:", input$flat_address, input$town, input$flat_model, input$flat_type, input$floor_area, input$storey, input$lease_commence_date)
    })
  })
  output$geoOutput <- renderText({ "Map will be displayed here." })
  
  output$priceOutput <- renderText({ "Predicted price will be shown here." })
  
})

