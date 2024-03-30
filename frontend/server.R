#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  observeEvent(input$submit, {
    # This is where you can process the inputs and update the outputs.
    # For example:
    output$output <- renderText({
      paste("You have selected:", input$flat_address, input$town, input$flat_model, input$flat_type, input$floor_area, input$storey, input$lease_commence_date)
    })
  })
}

