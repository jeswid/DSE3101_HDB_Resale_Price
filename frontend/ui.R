library(shiny)
library(leaflet)
library(shinyjs)
library(plotly)
library(shinydashboard)

min_sqm <- min(all_address_pred$floor_area_sqm, na.rm = TRUE)
max_sqm <- max(all_address_pred$floor_area_sqm, na.rm = TRUE)




ui <- dashboardPage(
  dashboardHeader(title = "HDB PROJECT"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        /* Header and Titles CSS */
        .main-header .logo {
          background-color: #FFFFFF !important; /* White background */
          color: #000000 !important; /* Black text */
        }
        .main-header .navbar {
          background-color: #FFFFFF !important; /* White background */
        }
        .main-header .navbar .sidebar-toggle {
          color: #000000 !important; /* Black text */
        }
        
        /* Sidebar CSS */
        .main-sidebar {
          background-color: #8B0000 !important; /* Dark red background */
        }
        .sidebar-menu li > a {
          color: #ffffff !important; /* White text for better contrast */
        }
        .sidebar-menu .treeview-menu > li > a {
          color: #ffffff !important;
        }
        .sidebar-menu li.active > a {
          border-left-color: #ffffff !important;
        }
        .sidebar-menu li.header {
          color: #ffffff !important;
        }
        
        /* Custom Homepage CSS */
        .content-wrapper {
          background-color: #FFFFF !important; /* Cream background for the home page */
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Geospatial Analysis", tabName = "geospatial", icon = icon("map")),
      menuItem("Predicted Price", tabName = "predicted", icon = icon("dollar")),
      menuItem("Price Trend", tabName = "forecasted", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* CSS to change the sidebar background color */
        .main-sidebar {
          background-color: #660000 !important;
        }
        /* Adjusting text and icon color for better visibility */
        .sidebar-menu li > a {
          color: #ffffff !important;
        }
        .sidebar-menu .treeview-menu > li > a {
          color: #ffffff !important;
        }
        .sidebar-menu li.active > a {
          border-left-color: #ffffff !important;
        }
        .sidebar-menu li.header {
          color: #ffffff !important;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       uiOutput("homeOutput"))
              )
      ),
      tabItem(tabName = "geospatial",
              fluidRow(
                column(9,
                       textOutput("intro2"),
                       verbatimTextOutput("geoSelectionOutput"),
                       leafletOutput("map", width = "100%", height = "600px"), 
                       DTOutput("mrt_table"),
                       DTOutput("hawkers_table"),
                       DTOutput("sch_table"),
                       DTOutput("supermarket_table"),
                       DTOutput("hospitals_table")
                ),
                column(3,
                       div(id = "sidebar", class = "well",
                           # Add a dropdown selection box for towns
                           selectInput("town", "Type or Select Town", choices = unique(c("ALL TOWNS", all_address$town)), selected = "ALL TOWNS"),
                           selectInput("addressM","Type or Select Postal Code", selected = "", choices = sort(unique(all_address$postal), decreasing = TRUE)),
                           actionButton("submitmap", "Click to zoom 🔎", class = "btn-primary")
                       )
                )
              )
      ),
      tabItem(tabName = "predicted",
              fluidRow(
                column(12, align = "center",
                       div(style = "width: 50%;", # Set the width of the container
                           textOutput("intro"),
                           sliderInput("floor_area_sqm", "Desired Square Meter",
                                       min =  min_sqm, max = max_sqm,
                                       value = round((min_sqm + max_sqm) / 2), round = TRUE),              
                           selectInput("address", "Postal Code", choices = sort(unique(laty$postal), decreasing = TRUE)),
                           selectInput("flat_modelM", "Flat Model", choices = c('Model A', 'Improved', 'Premium Apartment', 'Standard',
                                                                                'New Generation', 'Maisonette', 'Apartment', 'Simplified',
                                                                                'Model A2', 'DBSS', 'Terrace', 'Adjoined flat', 'Multi Generation',
                                                                                '2-room', 'Executive Maisonette', 'Type S1S2'), 
                                       selected = "Model A"),
                           selectInput("flat_type", "Flat Type", choices = c('2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE'), selected = "4 ROOM"),
                           sliderInput("storey","Desired Level",min = 1, max = 50,value = 1,round = TRUE),
                           actionButton("submitprice", "Submit HDB 🔎", class = "btn-primary"),
                           textOutput("priceOutput")
                       )
                )
              )),
      tabItem(tabName = "forecasted",
              fluidRow(
                column(12, align = "center",
                       textOutput("intro1"),
                       verbatimTextOutput("priceOutputF"),
                       plotlyOutput("forecastChart"),
                       sliderInput("floor_area_sqmF", "Desired Square Meter",
                                   min =  min_sqm, max = max_sqm,
                                   value = round((min_sqm + max_sqm) / 2), round = TRUE),
                       selectInput("addressF", "Postal Code", choices = sort(unique(laty$postal), decreasing = TRUE)),
                       selectInput("flat_modelMF", "Flat Model", choices = c('Model A', 'Improved', 'Premium Apartment', 'Standard',
                                                                             'New Generation', 'Maisonette', 'Apartment', 'Simplified',
                                                                             'Model A2', 'DBSS', 'Terrace', 'Adjoined flat', 'Multi Generation',
                                                                             '2-room', 'Executive Maisonette', 'Type S1S2'), 
                                   selected = "Model A"),
                       selectInput("flat_typeF", "Flat Type", choices = c('2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM', 'EXECUTIVE'), selected = "4 ROOM"),
                       sliderInput("storeyF","Desired Level",min = 1, max = 50,value = 1,round = TRUE),
                       actionButton("submitforecast", "Submit HDB 🔎", class = "btn-primary")
                )
              )
      )
    )
  )
) 
