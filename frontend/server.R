
library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
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
  

  
}

