library(shiny)
library(leaflet)

custom_css <- HTML(
  "<style>
    /* General Sidebar Styles */
    .sidebar {
      position: fixed;
      top: 0;
      bottom: 0;
      left: 0;
      z-index: 100;
      padding: 20px;
      overflow-x: hidden;
      overflow-y: auto;
      background-color: #343a40;
      color: #f8f9fa;
      width: 250px;
    }
    
    .sidebar .sidebar-sticky {
      position: relative;
      top: 0;
      height: calc(100vh - 48px);
      padding-top: 0.5rem;
    }
    
    /* Navigation Item Styles */
    .sidebar .nav .nav-item {
      font-weight: bold;
      color: #f8f9fa;
    }
    
    .sidebar .nav .nav-item .nav-link {
      border-radius: 0;
    }
    
    .sidebar .nav .nav-item .nav-link.active {
      background-color: #495057;
    }
    
    /* Input and Button Styles */
    .form-control, .btn {
      border-radius: 0.25rem;
      margin-bottom: 10px;
    }
    
    .form-control {
      background-color: #ffffff;
      color: #495057;
    }
    
    .btn {
      color: #f8f9fa;
      background-color: #007bff;
      border-color: #007bff;
      margin-top: 5px;
      width: 100%;
    }
  </style>"
)


ui <- fluidPage(
  tags$head(custom_css),
  div(id = "wrapper",
      # Custom Sidebar
      div(class = "sidebar bg-dark",
          div(class = "sidebar-sticky",
              h4("Inputs", class = "sidebar-heading"),
              textInput("flat_address", "Flat Address or Postal Code", value = "988B BUANGKOK GREEN"),
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
      # Page Content
      div(class = "page-content-wrapper",
          fluidRow(
            column(10, offset = 3,
                   tabsetPanel(
                     id = "main-tabs",
                     tabPanel("Home", textOutput("homeOutput")),
                     tabPanel("Geospatial Analysis", textOutput("geoOutput"), leafletOutput("map", width = "100%", height = "400px")),
                     tabPanel("Predicted Price", textOutput("priceOutput"))
                     # ... Add more tabs here
                   )
            )
          )
      )
  )
)



