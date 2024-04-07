library(shiny)
library(leaflet)
library(shinyjs)

min_sqm <- min(all_address_pred$floor_area_sqm, na.rm = TRUE)
max_sqm <- max(all_address_pred$floor_area_sqm, na.rm = TRUE)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
    
      .navbar-default {
        background-color: #FFFFFF;  /* White background */
        border-color: #FFB6C1;  /* Light red border */
      }
      .navbar-default .navbar-brand {
        color: #FFB6C1;  /* Light red color for the navbar brand (title) */
      }
      .navbar-default .navbar-nav > li > a {
        color: #FFB6C1;  /* Light red color for nav links */
      }
      .well {
        background-color: #FFFFFF;  /* White background for sidebar */
        border: 1px solid #FFB6C1;  /* Light red border for sidebar */
      }
      .btn-primary {
        background-color: #FFB6C1;  /* Light red background for buttons */
        border-color: #FFB6C1;  /* Light red border for buttons */
      }
      .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
        background-color: #FFC0CB; /* Lighter red when hovered, focused or active */
        border-color: #FFC0CB;
      }
      /* Additional styles can be added here */
    ")),
  navbarPage(
    id = "tabs",  # Important: set an ID for the navbarPage
    title = "Visualizing and Predicting Singapore HDB Resale Prices",
    tabPanel("Home", value = "Home",
             uiOutput("homeOutput") ),
    tabPanel("Geospatial Analysis", value = "GeospatialAnalysis",
             fluidRow(
               column(9,
                      leafletOutput("map", width = "100%", height = "600px"),
                      textOutput("geoSelectionOutput") # Change font-size value as needed
               ),

 # Map output
               column(3,
                      # Sidebar content for geospatial analysis goes here (e.g., inputs, action buttons, etc.)
                      # It will only be visible when the Geospatial Analysis tab is active
                      div(id = "sidebar", class = "well",
                          selectInput("addressM","Postal Code", choices = c(unique(all_address$postal))),
                          checkboxGroupInput("amenities", "Amenities", choices = c("MRT", "Primary Schools", "Hawker Centres", "Supermarkets")),
                          actionButton("submitmap", "Submit HDB 🔎", class = "btn-primary") ) ), ) ),
   
     
 tabPanel("Predicted Price", value = "PredictedPrice",
             
          
             fluidRow(
               column(12,
                      # Content for Predicted Price tab
                      textOutput("priceOutput") )),
             div(id = "sidebar", class = "well",
                 sliderInput("floor_area_sqm", "Desired Square Meter",
                             min =  min_sqm, max = max_sqm,
                             value = round((min_sqm + max_sqm) / 2), round = TRUE),              
                 selectInput("address","Postal Code", choices = c(unique(laty$postal))),
                 selectInput("flat_modelM", "Flat Model", choices = c('Model A', 'Improved', 'Premium Apartment', 'Standard',
                                                                      'New Generation', 'Maisonette', 'Apartment', 'Simplified',
                                                                      'Model A2', 'DBSS', 'Terrace', 'Adjoined flat', 'Multi Generation',
                                                                      '2-room', 'Executive Maisonette', 'Type S1S2'), 
                             selected = "Model A"),
                 selectInput("flat_type", "Flat Type", choices = c('2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE'), selected = "4 ROOM"),
                 sliderInput("storey","Desired Level",min = 1, max = 50,value = 1,round = TRUE),

                          actionButton("submitprice", "Submit HDB 🔎", class = "btn-primary") )),
    verbatimTextOutput("priceOutput") ))
 
)

  




