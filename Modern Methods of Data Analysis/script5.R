# Multivariate Adaptive Regression (MARS)
# Support Vector Machine (SVM)
# K-Nearest Neighbors (KNN)

library(doMC)
registerDoMC(8)
library(AppliedPredictiveModeling)
data(solubility)
ls(pattern = "^solT")

# install.packages("earth")
library(earth)

marsFit <- earth(solTrainXtrans, solTrainY)
marsFit
summary(marsFit)
plotmo(marsFit)

library(caret)

marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
set.seed(100)
marsTuned <- train(solTrainXtrans, solTrainY, method = "earth")
tuneGrid <- marsGrid
trControl <- trainControl(method = "cv")

marsTuned
varImp(marsTuned)

marsPredicted <- predict(marsTuned, solTestXtrans)
marsValues <- data.frame(obs = solTestY, pred = marsPredicted)
xyplot(solTestY ~ marsPredicted, data = marsValues, pch = 19)
defaultSummary(marsValues)


# Support Vector Machines
# install.packages("kernlab")
library(kernlab)

svmFit <- ksvm(x = solTrainXtrans, y = solTrainY,
               kernel = "rbfdot", kpar = "automatic",
               C = 1, epsilon = 0.1)

svmRTuned <- train(solTrainXtrans, solTrainY, method = "svmRadial",
                   preProcess = c("center", "scale"),
                   tuneLength = 14,
                   trControl = trainControl(method = "cv"))
# svmPTuned <- train(solTrainXtrans, solTrainY, method = "svmPol")
svmRTuned
svmRTuned$finalModel
svmPredicted <- predict(svmRTuned, solTestXtrans)
svmValues <- data.frame(obs = solTestY, pred = svmPredicted)
xyplot(solTestY ~ svmPredicted, data = svmValues, pch = 19)
RMSE(pred = svmPredicted, obs = solTestY)
R2(pred = svmPredicted, obs = solTestY, formula = "corr")

# K-Nearest Neighbors
knnDescr <- solTrainXtrans[, -nearZeroVar(solTrainXtrans)]
set.seed(100)
knnTune <- train(knnDescr, solTrainY, method = "knn",
                 preProc = c("center", "scale"),
                 tuneGrid = data.frame(.k = 1:20),
                 trControl = trainControl(method = "cv"))
knnTune
knnTestX <- solTestXtrans[, -nearZeroVar(solTestXtrans)]
knnPredicted <- predict(knnTune, knnTestX)
