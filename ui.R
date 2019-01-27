# User interface

library(shiny)
library(shinydashboard)
source("Modules.R")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("ML Model", tabName = "mlmod", icon = icon("circle"))
    )
  ),
  dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML(
      "
      \n /* logo */\n.skin-blue .main-header .logo {\n background-color: #d11141;
      \n }\n\n/* logo when hovered */\n .skin-blue .main-header .logo:hover {\n background-color: #d11141;
      \n }\n\n/* navbar (rest of the header) */\n .skin-blue .main-header .navbar {\n background-color: #d11141;
      \n }\n\n/* main sidebar */\n .skin-blue .main-sidebar {\n background-color: #00b159;
      \n }\n\n/* active selected tab in the sidebarmenu */\n .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{\n background-color: #00aedb;
      \n }\n\n/* other links in the sidebarmenu */\n .skin-blue .main-sidebar .sidebar .sidebar-menu a{\n background-color: #00ff00;
      \n color: #000000;
      \n }\n\n/* other links in the sidebarmenu when hovered */\n .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{\n background-color: #f37735;
      \n }\n/* toggle button when hovered  */ \n .skin-blue .main-header .navbar .sidebar-toggle:hover{\n background-color: #f37735;
      \n }\n
      
      "
    ))),
    tabItems(
      tabItem( # First tab content
        tabName = "mlmod",
        tabsetPanel(
          tabPanel(
            "Boosted regression",
            tabsetPanel(
              tabPanel(
                "Training",
                fluidPage(
                  # To change color inside of sidebar, move lines below into tags$style()
                  # #sidebar {
                  # background-color: #dec4de;
                  #   }
                  tags$head(tags$style(
                    HTML('
                         body, label, input, button, select {
                         font-family: "serif";
                         }')
                  )),
                  fluidRow(
                    sidebarLayout(
                      sidebarPanel(
                        id = "sidebar",
                        fluidRow(
                          column(
                            width = 6,
                            tags$head(tags$style(".progress-bar{background-color:#ff6f69;}")),
                            csvFileInput("file1", "User data (.csv format)"),
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #96ceb4;}")),
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-grid-text { font-size: 10pt;}")),
                            sliderInput("split", "Split ratio", min = 0.1, max = 1, value = 0.7, step = 0.01),
                            numericInput("seedN", "Seed number", min = 1, max = 100000000, value = 1234),
                            numericInput("cv.N", "Number of iter to find best pars for CV", min = 1, max = 10000, value = 100, step = 1),
                            numericInput("cv.Nrds", "Number of trees for boosted regression", min = 1, max = 10000, value = 300, step = 1),
                            numericInput("cv.Nflds", "Number of K folds for CV", min = 1, max = 10000, value = 10, step = 1),
                            numericInput("Cores", "Number of cores", value = 6, min = 1, max = 16),
                            actionButton("accept", "Accept", icon = icon("random"))
                          ),
                          column(
                            width = 6,
                            uiOutput("srcCol")
                          )
                        )
                      ),
                      mainPanel(
                        fluidRow(
                          tabsetPanel(
                            tabPanel(
                              "Data",
                              column(
                                width = 12,
                                DTOutput("dat.Source"),
                                style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;",
                                withSpinner(verbatimTextOutput("cvXgb"), color = "#ffc425"),
                                plotOutput('cvConf')
                              )
                            ),
                            tabPanel(
                              "Plots",
                              fluidRow(
                                column(
                                  width = 9,
                                  br(),
                                  withSpinner (plotOutput("srcCor"))
                                ),
                                column(
                                  width = 3,
                                  sliderInput("R", "R2", value = 0.6, min = 0, max = 0.99, step = 0.1, animate = T),
                                  selectInput("method", "Method", c(
                                    "circle", "square", "ellipse", "number", "shade",
                                    "color", "pie"
                                  )),
                                  selectInput("type", "Type", c("full", "lower", "upper")),
                                  sliderInput("tl.cex", "FontSize", value = 0.5, min = 0.1, max = 1.5, step = 0.1)
                                ),
                                column (
                                  width = 12,
                                  br(),
                                  withSpinner (plotOutput('srcDCor', height = "1000px"))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Testing",
                fluidPage(
                  fluidRow(
                    sidebarLayout(
                      sidebarPanel(
                        csvFileInput("file2", "Target data"),
                        actionButton("accept2", "Accept", icon = icon("random"))
                      ),
                      mainPanel(
                        DTOutput("dat.Target"),
                        style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;",
                        withSpinner(plotOutput("p1"), color = "#ffc425"),
                        withSpinner(plotOutput("p2"), color = "#ffc425")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

# shinyApp(ui, server)
