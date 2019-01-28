library(shiny)
library(DT)

# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label,
      multiple = TRUE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv"
      )
    )
  )
}


# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath)
  })
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  # Return the reactive that yields the data frame
  return(dataframe)
}


# Used to select column of data.table or a data frame
# Module UI function
columnChooserUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("columnChooserUI"))
}


DT_tab <- function(id) {
  ns <- NS(id)
  withSpinner(DTOutput(ns("DT_tab")))
}


mat_par <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("corR")),
    uiOutput(ns("corMethod")),
    uiOutput(ns("corType")),
    uiOutput(ns("tl.cex"))
  )
}


srcCor <- function(id) {
  ns <- NS(id)
  withSpinner(plotOutput(ns("srcCor")))
}


srcDCor <- function(id) {
  ns <- NS(id)
  withSpinner(plotOutput(ns("srcDCor"), height = "1000px"))
}


# Module server function
columnChooser <- function(input, output, session, data) {
  
  output$corR <- renderUI({
    ns <- session$ns
    sliderInput(ns("corR"), "R2", value = 0.6, min = 0, max = 0.99, step = 0.1, animate = T)
  })

  output$corMethod <- renderUI({
    ns <- session$ns
    selectInput(ns("corMethod"), "Method", c(
      "pie", "circle", "square", "ellipse", "number", "shade",
      "color"
    ))
  })

  output$corType <- renderUI({
    ns <- session$ns
    selectInput(ns("corType"), "Type", c( "lower","full", "upper"))
  })

  output$tl.cex <- renderUI({
    ns <- session$ns
    sliderInput(ns("tl.cex"), "FontSize", value = 1, min = 0.1, max = 1.5, step = 0.1)
  })

  clmns <- reactive({
    ns <- session$ns
    checkboxGroupInput(ns("col"), "Remove: ", names(data), selected = names(data))
  })

  # select columns for correlation plot
  output$columnChooserUI <- renderUI({
    clmns()
  })

  DT_tab <- reactive({
    if (length(input$col)<1){
      data
    } else {
      data[, input$col]
    }
  })
  
  # table output
  output$DT_tab <- renderDT({
    req(data)
    DT_tab ()
  })

  # Correlationp plot
  output$srcCor <- renderPlot({
    req(data)
    req (input$corR)
    dat <- na.omit(data)

    if (any(sapply(dat[, input$col], is.factor)) || any(sapply(dat[, input$col], is.character))) {
      "Please check data for character or factors"
    } else {
      M <- cor(dat[, input$col])
      M[M < input$corR & M > -input$corR] <- 0
      corrplot(M, method = input$corMethod, order = "hclust", input$corType, tl.cex = input$tl.cex, diag = FALSE)
    }
  }, height = "auto")

  # corrplot of src with distributions
  output$srcDCor <- renderPlot({
    req(data)
    if (length(input$col) < 2) {
    } else {
      dat <- na.omit(data)
      dat <- dat[, input$col]
      pairs.panels(dat,
        method = "pearson", # correlation method
        hist.col = "#00AFBB",
        density = TRUE, # show density plots
        ellipses = TRUE # show correlation ellipses
      )
    }
  })
  
  return (DT_tab)
}
