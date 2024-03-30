library(shiny)

ui <- fluidPage(
  titlePanel("Visualizing and Predicting Singapore HDB Resale Prices"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("flat_address", "Flat Address or Postal Code", value = "988B BUANGKOK GREEN"),
      
      selectInput("town", "Town", 
                  choices = c('ANG MO KIO', 'BEDOK', 'BISHAN', 'BUKIT BATOK', 'BUKIT MERAH',
                              'BUKIT TIMAH', 'CENTRAL AREA', 'CHOA CHU KANG', 'CLEMENTI',
                              'GEYLANG', 'HOUGANG', 'JURONG EAST', 'JURONG WEST',
                              'KALLANG/WHAMPOA', 'MARINE PARADE', 'QUEENSTOWN', 'SENGKANG',
                              'SERANGOON', 'TAMPINES', 'TOA PAYOH', 'WOODLANDS', 'YISHUN',
                              'LIM CHU KANG', 'SEMBAWANG', 'BUKIT PANJANG', 'PASIR RIS','PUNGGOL'),
                  selected = "SERANGOON"),
      
      selectInput("flat_model", "Flat Model", 
                  choices = c('Model A', 'Improved', 'Premium Apartment', 'Standard',
                              'New Generation', 'Maisonette', 'Apartment', 'Simplified',
                              'Model A2', 'DBSS', 'Terrace', 'Adjoined flat', 'Multi Generation',
                              '2-room', 'Executive Maisonette', 'Type S1S2'),
                  selected = "Model A"),
      
      selectInput("flat_type", "Flat Type", 
                  choices = c('2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE'),
                  selected = "4 ROOM"),
      
      sliderInput("floor_area", "Floor Area (sqm)", min = 34, max = 280, value = 93),
      
      selectInput("storey", "Storey", 
                  choices = c('01 TO 03','04 TO 06','07 TO 09','10 TO 12','13 TO 15',
                              '16 TO 18','19 TO 21','22 TO 24','25 TO 27','28 TO 30',
                              '31 TO 33','34 TO 36','37 TO 39','40 TO 42','43 TO 45',
                              '46 TO 48','49 TO 51'),
                  selected = "07 TO 09"),
      
      selectInput("lease_commence_date", "Lease Commencement Date", 
                  choices = as.character(rev(1966:2016)), 
                  selected = "1967"),
      
      actionButton("submit", "Submit HDB 🔎")
    ),
    
    mainPanel(
      # Using tabsetPanel to create tabs
      tabsetPanel(
        tabPanel("Map", 
                 # Placeholder for map output
                 textOutput("mapOutput") # You can replace this with your map rendering output like plotOutput("map") for a leaflet map
        ),
        tabPanel("Predicted Price", 
                 # Placeholder for predicted price output
                 textOutput("priceOutput") # You might want to replace this with your actual output element
        )
      )
    )
  )
)


