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
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Geospatial Analysis", tabName = "geospatial", icon = icon("map")),
      menuItem("Predicted Price", tabName = "predicted", icon = icon("dollar")),
      menuItem("Trend Analysis", tabName = "forecasted", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
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
                       verbatimTextOutput("geoSelectionOutput"),
                       leafletOutput("map", width = "100%", height = "600px"), 
                       tableOutput("mrt_table"), 
                       tableOutput("sch_table"), 
                       tableOutput("supermarket_table"), 
                       tableOutput("hawkers_table"),
                       tableOutput("hospitals_table")
                ),
                column(3,
                       div(id = "sidebar", class = "well",
                           selectInput("addressM","Type or Select Postal Code", selected = "", choices = sort(unique(all_address$postal), decreasing = TRUE)),
                           actionButton("submitmap", "Click to zoom 🔎", class = "btn-primary")
                       )
                )
              )
      ),
      tabItem(tabName = "predicted",
              fluidRow(
                column(12,
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
      ),
      tabItem(tabName = "forecasted",
              fluidRow(
                column(12,
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
