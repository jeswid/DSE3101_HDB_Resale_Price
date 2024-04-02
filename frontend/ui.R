library(shiny)
library(leaflet)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'tabs') {
          if (['GeospatialAnalysis', 'PredictedPrice'].includes(event.value)) {
            $('#sidebar').show();
          } else {
            $('#sidebar').hide();
          }
        }
      });
    "))
  ),
  navbarPage(
    id = "tabs",  # Important: set an ID for the navbarPage
    title = "Visualizing and Predicting Singapore HDB Resale Prices",
    tabPanel("Home", value = "Home",
             fluidRow(
               column(12,
                      # Content for Home tab
                      textOutput("homeOutput")
               )
             )
    ),
    tabPanel("Geospatial Analysis", value = "GeospatialAnalysis",
             fluidRow(
               column(9,
                      leafletOutput("map", width = "100%", height = "600px") # Map output
               ),
               column(3,
                      # Sidebar content for geospatial analysis goes here (e.g., inputs, action buttons, etc.)
                      # It will only be visible when the Geospatial Analysis tab is active
                      div(id = "sidebar", class = "well",
                          selectInput("town", "Town", choices = c('ANG MO KIO', 'BEDOK', 'BISHAN', 'BUKIT BATOK', 'BUKIT MERAH',
                                                                  'BUKIT TIMAH', 'CENTRAL AREA', 'CHOA CHU KANG', 'CLEMENTI',
                                                                  'GEYLANG', 'HOUGANG', 'JURONG EAST', 'JURONG WEST',
                                                                  'KALLANG/WHAMPOA', 'MARINE PARADE', 'QUEENSTOWN', 'SENGKANG',
                                                                  'SERANGOON', 'TAMPINES', 'TOA PAYOH', 'WOODLANDS', 'YISHUN',
                                                                  'LIM CHU KANG', 'SEMBAWANG', 'BUKIT PANJANG', 'PASIR RIS', 'PUNGGOL'), 
                                      selected = "SERANGOON"),
                          selectInput("flat_model", "Flat Model", choices = c('Model A', 'Improved', 'Premium Apartment', 'Standard',
                                                                              'New Generation', 'Maisonette', 'Apartment', 'Simplified',
                                                                              'Model A2', 'DBSS', 'Terrace', 'Adjoined flat', 'Multi Generation',
                                                                              '2-room', 'Executive Maisonette', 'Type S1S2'), 
                                      selected = "Model A"),
                          selectInput("flat_type", "Flat Type", choices = c('2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE'), selected = "4 ROOM"),
                          selectInput("amenities", "Amenities", choices = c("Primary School", "Shopping Centre", "Food Court", "Gym", "Community Center", "Junior College"), selected = "Primary School"),
                          actionButton("submit", "Submit HDB 🔎", class = "btn-primary")
                      )
               ),
                          
                          
                          
                          
                          
                          
                          
                          
                      )
               )
             ),

    tabPanel("Predicted Price", value = "PredictedPrice",
             fluidRow(
               column(12,
                      # Content for Predicted Price tab
                      textOutput("priceOutput") ) )))
  




