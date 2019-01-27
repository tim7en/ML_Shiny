library(caret)
library(xgboost)
library(dplyr)
library(e1071)
library(shiny)
library(shinydashboard)
source("pcg.R")

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
                            fileInput("file4", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"
                                      )
                            ),
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #96ceb4;}")),
                            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-grid-text { font-size: 10pt;}")),
                            sliderInput("split", "Split ratio", min = 0.1, max = 1, value = 0.7, step = 0.01),
                            
                            numericInput("seedN", "Seed number", min = 1, max = 100000000, value = 1234),
                            numericInput("cv.N", "Number of iter to find best pars for CV", min = 1, max = 10000, value = 100, step = 1),
                            numericInput("cv.Nrds", "Number of iter for boosted regression CV", min = 1, max = 10000, value = 300, step = 1),
                            numericInput("cv.Nflds", "Number of K folds for CV", min = 1, max = 10000, value = 10, step = 1),
                            numericInput("Cores", "Number of cores", value = 6, min = 1, max = 16),
                            actionButton("accept", "Accept", icon = icon("random"))
                          ),
                          column(
                            width = 6,
                            
                            uiOutput("colChoices")
                          )
                        )
                      ),
                      mainPanel(
                        DTOutput("x50"),
                        style = "height:'auto'; overflow-y: scroll;overflow-x: scroll;",
                        withSpinner(verbatimTextOutput("tab1"), color = "#ffc425")
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
                        fileInput("file5", "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"
                                  )
                        ),
                        actionButton("accept2", "Accept", icon = icon("random"))
                      ),
                      mainPanel(
                        DTOutput("x51"),
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

server <- function(input, output) {
  x50 <- reactive({
    req(input$file4)
    df <- read.csv(input$file4$datapath,
                   header = T,
                   sep = ",",
                   quote = '"'
    )
    na.omit(df)
  })
  
  output$colChoices <- renderUI({
    req(x50())
    datas <- x50()
    checkboxGroupInput("ColSourcenames",
                       "Remove Column:",
                       names(datas)[-c(1, 2)],
                       selected = names(datas)[-c(1, 2)]
    )
  })
  
  output$x50 <- renderDT({
    x50()
  })
  
  x51 <- reactive({
    req(input$file5)
    df <- read.csv(input$file5$datapath,
                   header = T,
                   sep = ",",
                   quote = '"'
    )
    na.omit(df)
  })
  
  output$x51 <- renderDT({
    x51()
  })
  
  tab1 <- reactive({
    req(x50())
    # req (input$colChoices)
    dat <- x50()
    
    rownames(dat) <- dat[, 1]
    dat <- dat[, -1]
    dat[, 1] <- as.numeric(dat[, 1]) - 1
    dat <- cbind(dat[, 1], dat[, input$ColSourcenames])
    
    train_index <- sample(1:nrow(dat), nrow(dat) * input$split)
    
    data_variables <- as.matrix(dat[, -1])
    data_label <- dat[, 1]
    data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
    
    # 1. step, test for cv
    # split train data and make xgb.DMatrix
    train_data <- data_variables[train_index, ]
    train_label <- data_label[train_index]
    train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
    
    
    test_data <- data_variables[-train_index, ]
    test_label <- data_label[-train_index]
    test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
    
    best_param <- list()
    best_seednumber <- input$seedN
    best_logloss <- Inf
    best_logloss_index <- 0
    
    for (iter in 1:input$cv.N) {
      xgb_params <- list(
        objective = "multi:softprob",
        eval_metric = "mlogloss",
        num_class = 4,
        max_depth = sample(6:10, 1),
        eta = runif(1, .01, .3),
        gamma = runif(1, 0.0, 0.2),
        subsample = runif(1, .6, .9),
        colsample_bytree = runif(1, .5, .8),
        min_child_weight = sample(1:40, 1),
        max_delta_step = sample(1:10, 1)
      )
      cv.nround <- input$cv.Nrds
      cv.nfold <- input$cv.Nflds
      seed.number <- sample.int(10000, 1)[[1]]
      set.seed(seed.number)
      mdcv <- xgb.cv(
        data = train_matrix, params = xgb_params, nthread = input$Cores,
        nfold = cv.nfold, nrounds = cv.nround,
        verbose = F, early_stopping_rounds = 8, maximize = FALSE, prediction = T
      )
      
      min_logloss <- min(mdcv$evaluation_log[, test_mlogloss_mean])
      min_logloss_index <- which.min(mdcv$evaluation_log[, test_mlogloss_mean])
      
      if (min_logloss < best_logloss) {
        best_logloss <- min_logloss
        best_logloss_index <- min_logloss_index
        best_seednumber <- seed.number
        best_param <- xgb_params
      }
    }
    
    set.seed(best_seednumber)
    cv_model <- xgb.cv(
      params = best_param,
      data = train_matrix,
      nrounds = best_logloss_index,
      nfold = cv.nfold,
      verbose = FALSE,
      nthread = input$Cores,
      prediction = TRUE
    )
    
    OOF_prediction <- data.frame(cv_model$pred) %>%
      mutate(
        max_prob = max.col(., ties.method = "last"),
        label = train_label + 1
      )
    
    j <- confusionMatrix(factor(OOF_prediction$max_prob),
                         factor(OOF_prediction$label),
                         mode = "everything"
    )
    return(c(best_logloss, best_logloss_index, best_seednumber, j, c(best_param)))
  })
  
  tab2 <- eventReactive(input$accept2, {
    req(tab1())
    req(x50())
    vals <- tab1()
    set.seed(vals[[3]]) # best seed
    
    dat <- x50()
    
    rownames(dat) <- dat[, 1]
    dat <- dat[, -1]
    sourceNames <- unique(dat[, 1])
    dat[, 1] <- as.numeric(dat[, 1]) - 1
    
    dat <- cbind(dat[, 1], dat[, input$ColSourcenames])
    
    train_index <- sample(1:nrow(dat), nrow(dat) * 1) # changed to 1, since entire dataset
    data_variables <- as.matrix(dat[, -1])
    data_label <- dat[, 1]
    data_matrix <- xgb.DMatrix(data = as.matrix(dat), label = data_label)
    
    # 1. step, test for cv
    # split train data and make xgb.DMatrix
    train_data <- data_variables[train_index, ]
    train_label <- data_label[train_index]
    train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
    
    bst_model <- xgb.train(
      params = vals[5:length(vals)],
      data = train_matrix,
      nrounds = vals[[2]]
    )
    
    target <- x51()
    target <- target [, -1]
    target[, 1] <- 1
    target[, 1] <- as.numeric(target[, 1]) - 1
    target <- cbind(target[, 1], target[, input$ColSourcenames])
    
    test_data <- as.matrix(target [, -1])
    test_label <- target [, 1]
    test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
    numberOfClasses <- length(unique(dat[, 1]))
    
    test_pred <- predict(bst_model, newdata = test_matrix)
    
    test_prediction <- matrix(test_pred,
                              nrow = numberOfClasses,
                              ncol = length(test_pred) / numberOfClasses
    ) %>%
      t() %>%
      data.frame() %>%
      mutate(
        label = test_label + 1,
        max_prob = max.col(., "last")
      )
    
    clNames <- colnames(dat[, -1])
    
    importance_matrix <- xgb.importance(feature_names = clNames, model = bst_model)
    gp <- xgb.ggplot.importance(importance_matrix)
    
    
    names(test_prediction) <- c(as.character(unique(dat[, 1])), "label", "maxProb")
    
    colnames(test_prediction[, 1:ncol(test_prediction)]) <- c(as.character(unique(dat[, 1])))
    
    datplot <- t(as.matrix(test_prediction[, 1:numberOfClasses]))
    
    
    rownames(datplot) <- as.character(sourceNames)
    datas <- datplot
    datas <- (melt(datas))
    colnames(datas) <- c("Class", "Id", "Proportion")
    datas[, 2] <- as.factor(datas[, 2])
    
    p <- ggplot(datas, aes(x = Id, y = Proportion, fill = Class)) +
      geom_bar(stat = "identity", position = "fill") + ggtitle("Proportion of sediment fluxes")
    
    
    d <- list(gp, p)
    return(d)
  })
  
  output$p1 <- renderPlot({
    tab2()[[1]]
  })
  
  output$p2 <- renderPlot({
    tab2()[[2]]
  })
  
  tab1_comp <- eventReactive(input$accept, {
    d <- tab1()
  })
  
  output$tab1 <- renderPrint({
    dat <- tab1_comp()
    dat[5:7]
  })
}

shinyApp(ui, server)
