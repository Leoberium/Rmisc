library(AppliedPredictiveModeling)
library(caret)
library(doMC)
data(solubility)
ls(pattern = "^solT")
registerDoMC(8)


# Neural networks

tooHigh <- findCorrelation(cor(solTrainXtrans), cutoff = .75)
trainXnnet <- solTrainXtrans[, -tooHigh]
testXnnet <- solTestXtrans[, -tooHigh]

# Basic Neural Network Function
library(nnet)
nnetFit <- nnet(trainXnnet, solTrainY, size = 5,
                decay = 0.01,
                linout = TRUE,
                trace = FALSE,
                maxit = 500,
                MaxNWts = 5 * (ncol(trainXnnet) + 1) + 5 + 1)
nnetPred <- predict(nnetFit, testXnnet)
nnetValues <- data.frame(obs = solTestY, pred = nnetPred)
defaultSummary(nnetValues) # R2 - 0.70, not consistent

# avNNet function (caret)
nnetAvg <- avNNet(trainXnnet, solTrainY, size = 5,
                  decay = 0.01,
                  repeats = 5,
                  linout = TRUE,
                  trace = FALSE,
                  maxit = 500,
                  MaxNWts = 5 * (ncol(trainXnnet) + 1) + 5 +1)
nnetAvgPred <- predict(nnetAvg, testXnnet)
nnetAvgValues <- data.frame(obs = solTestY, pred = nnetPred)
defaultSummary(nnetAvgValues) # R2 - 0.70

# train function (caret package)
set.seed(100)
indx <- createFolds(solTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
nnetGrid <- expand.grid(decay = c(0),
                        size = c(4),
                        bag = FALSE)
nnetTune <- train(x = trainXnnet, y = solTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProcess = c("center", "scale"),
                  linout = TRUE,
                  trace = TRUE,
                  MaxNWts = 13 * (ncol(trainXnnet) + 1) + 13 + 1,
                  maxit = 500)
nnetTrainPred <- predict(nnetTune, testXnnet)
nnetTrainValues <- data.frame(obs = solTestY, pred = nnetTrainPred)
defaultSummary(nnetTrainValues) # R2 - 0.83
