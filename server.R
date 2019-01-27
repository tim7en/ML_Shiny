source("pcg.R")
source ('Modules.R')
library(caret)
library(xgboost)
library(dplyr)
library(e1071)
library(shiny)
library(shinydashboard)
source ('ui.R')

#Background side of the server
server <- function(input, output) {
  
  #upload source data
  dat.Source <- callModule(csvFile, "file1",
                          stringsAsFactors = FALSE)
  
  #give user an option to pick columns from training to remove
  output$srcCol <- renderUI({
    req(dat.Source())
    dat.Source <- dat.Source()
    checkboxGroupInput("ColSourcenames",
      "Remove Column:",
      names(dat.Source)[-c(1, 2)],
      selected = names(dat.Source)[-c(1, 2)]
    )
  })

  #omit na before outputting source table
  output$dat.Source <- renderDT({
    na.omit (dat.Source())
  })
  
  #corrplot of src
  output$srcCor <- renderPlot({
    req(dat.Source())
    dat <- na.omit(dat.Source())
    M <- cor(dat[,-c(1,2)])
    M[M < input$R & M > -input$R] <- 0
    corrplot(M, method = input$method, order = "hclust", input$type, tl.cex = input$tl.cex, diag = FALSE)
    
  }, height = 'auto')
  
  #corrplot of src with distributions
  output$srcDCor <- renderPlot ({
    req (dat.Source ())
    req (input$ColSourcenames)
    if (length (input$ColSourcenames) < 2) {
      
    } else {
      dat <- na.omit (dat.Source ())
      dat <- dat[,input$ColSourcenames]
      pairs.panels(dat, 
                   method = "pearson", # correlation method
                   hist.col = "#00AFBB",
                   density = TRUE,  # show density plots
                   ellipses = TRUE # show correlation ellipses
      )
    }
  })
  

  #upload target data
  dat.Target <- callModule(csvFile, "file2",
                           stringsAsFactors = FALSE)
  
  
  #omit na before outputting target data
  output$dat.Target <- renderDT({
    na.omit(dat.Target())
  })

  
  #gradient boosting function with input variables used for training
  cvXgb <- reactive({
    req (input$ColSourcenames)
    req (dat.Source())
    dat <- dat.Source()
    rownames(dat) <- dat[, 1]
    dat <- dat[, -1]
    dat[, 1] <- as.numeric(dat[, 1]) - 1
    numClasses <- length(unique(dat[,1])) #number of unique classes in the data frame, always check it!
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
        num_class = numClasses, #Should be dynamic!
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

  
  
  #gradient boosting function used to predict target
  tarPred <- eventReactive(input$accept2, {
    req(cvXgb())
    req(dat.Source())
    cvPars <- cvXgb()
    set.seed(cvPars[[3]]) # best seed
    dat <- dat.Source()
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
      params = cvPars[5:length(cvPars)],
      data = train_matrix,
      nrounds = cvPars[[2]]
    )

    target <- dat.Target()
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

  #render multiple plots from the list output
  output$p1 <- renderPlot({
    tarPred()[[1]]
  })

  output$p2 <- renderPlot({
    tarPred()[[2]]
  })

  cvXgb_comp <- eventReactive(input$accept, {
    d <- cvXgb()
  })
  
  #print cross validation data
  output$cvXgb <- renderPrint({
    dat <- cvXgb_comp()
    dat[[6]][1:2]
  })
  
  output$cvConf <- renderPlot ({
    req (cvXgb_comp())
    dat <- cvXgb_comp()
    x <- melt(dat[[5]])
    
    ggplot(data =  x, mapping = aes(x = Prediction, y = Reference)) +
      geom_tile(aes(fill = value), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", value)), vjust = 1) +
      scale_fill_gradient(low = "lightblue", high = "salmon") +
      theme_bw() + theme(legend.position = "none")
    
  })
}
