#Timur Sabitov
# set up example data set
library("titanic")
data(titanic_train)
str(titanic_train)


outcome <- 'Survived'
target <- 1
shouldBeCategorical <- c('PassengerId', 'Pclass', 'Parch')
for(v in shouldBeCategorical) {
  titanic_train[[v]] <- as.factor(titanic_train[[v]])
}
tooDetailed <- c("Ticket", "Cabin", "Name", "PassengerId")
vars <- setdiff(colnames(titanic_train), c(outcome, tooDetailed))

dTrain <- titanic_train


library("xgboost")
library("sigr")
library("WVPlots")
library("vtreat")


set.seed(4623762)
crossValPlan <- vtreat::kWayStratifiedY(nrow(dTrain), 
                                        10, 
                                        dTrain, 
                                        dTrain[[outcome]])

evaluateModelingProcedure <- function(xMatrix, outcomeV, crossValPlan) {
  preds <- rep(NA_real_, nrow(xMatrix))
  for(ci in crossValPlan) {
    nrounds <- 1000
    cv <- xgb.cv(data= xMatrix[ci$train, ],
                 label= outcomeV[ci$train],
                 objective= 'binary:logistic',
                 nrounds= nrounds,
                 verbose= 0,
                 nfold= 5)
    #nrounds  <- which.min(cv$evaluation_log$test_rmse_mean) # regression
    nrounds  <- which.min(cv$evaluation_log$test_error_mean) # classification
    model <- xgboost(data= xMatrix[ci$train, ],
                     label= outcomeV[ci$train],
                     objective= 'binary:logistic',
                     nrounds= nrounds,
                     verbose= 0)
    preds[ci$app] <-  predict(model, xMatrix[ci$app, ])
  }
  preds
}
#Our preferred way to encode data is to use the vtreat package in the "no variables mode" shown below (differing from the powerful "y aware" modes we usually teach).

set.seed(4623762)
tplan <- vtreat::designTreatmentsZ(dTrain, vars, 
                                   minFraction= 0,
                                   verbose=FALSE)
# restrict to common varaibles types
# see vignette('vtreatVariableTypes', package = 'vtreat') for details
sf <- tplan$scoreFrame
newvars <- sf$varName[sf$code %in% c("lev", "clean", "isBAD")] 
trainVtreat <- as.matrix(vtreat::prepare(tplan, dTrain, 
                                         varRestriction = newvars))
print(dim(trainVtreat))

dTrain$predVtreatZ <- evaluateModelingProcedure(trainVtreat,
                                                dTrain[[outcome]]==target,
                                                crossValPlan)
sigr::permTestAUC(dTrain, 
                  'predVtreatZ',
                  outcome, target)


WVPlots::ROCPlot(dTrain, 
                 'predVtreatZ', 
                 outcome, target, 
                 'vtreat encoder performance')


