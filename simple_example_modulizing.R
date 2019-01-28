library(shiny)

setUnitUI <- function(id) {
  ns <- NS(id)
  selectInput(ns('unit'), 'unit', c('km', 'mile'))
}

setValueUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns('dynamicSlider'))
}

showValueUI <- function(id) {
  ns <- NS(id)
  textOutput(ns('value'))
}

ui <- fluidPage(
  setUnitUI('unit'),
  setValueUI('value'),
  showValueUI('show')
)



setUnitModule <- function(input, output, session) { #This is used to get units, that are used in the next function
  reactive(input$unit)
}

setValueModule <- function(input, output, session, unitGetter) { #This is used to display renderUI slider based on the units picked
  
  output$dynamicSlider <- renderUI({
    ns <- session$ns
    unit <- unitGetter()
    if (unit == 'km') {
      sliderInput(ns('pickValue'), paste('Pick value in', unit), 
                  min=0, max=150, value=0)
    } else {
      sliderInput(ns('pickValue'), paste('Pick value in', unit), 
                  min=0, max=100, value=0)
    }
  })
  
  reactive(input$pickValue)
}




showValueModule <- function(input, output, session, unitGetter, valueGetter) {
  output$value <- renderText(paste('You chose', valueGetter(), unitGetter()))
}



server <- function(input, output, session) {
  unitGetter <- callModule(setUnitModule, 'unit')
  valueGetter <- callModule(setValueModule, 'value', unitGetter)
  callModule(showValueModule, 'show', unitGetter, valueGetter)
}


shinyApp(ui, server, options=list(launch.browser=TRUE))