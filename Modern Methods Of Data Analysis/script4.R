# Linear Regression Models

# How do we evaluate our prediction:
# - Root Mean Squared Error as the average distance between the observed values and model predictions
# - Coefficient of determination R^2 (Pearson correlation)

# Why is the word "regression"?
# Pearson Data - correlation between son's height and father's height
# Regression towards mediocrity in hereditary stature

# Data - chemical structure and solubility
# Propertes:
# - 208 binary "fingerprints" that indicate the presence or absence of a particular chemical substructure
# - 16 descriptors, such as the number of bonds or the number of bromine atoms
# - 4 continuous descriptors such as molecular weight or surface area

# Ordinary Linear Regression
# Finds the plane that minimizes the sum-of-squared errors (SSE) between the observed and predicted response

# Principal component regression (PCR)
# PCR = PCA + Regression
# Dimension reduction, then regression
# But! Specifically, dimension reduction via PCA does not necessarily produce new predictors that explain the response.

# Partial Least Square (PLS)
# - PLS can be viewed as a supervised dimension reduction procedure 
# - PCR is an unsupervised procedure

library(AppliedPredictiveModeling)
data(solubility)
ls(pattern = "^solT")
head(solTestX)

library(caret)

trainingData <- solTrainXtrans
trainingData$Solubility <- solTrainY
lmFitAllPredictors <- lm(Solubility ~., data = trainingData)
# Solubility ~. - we predict Solubility, using ALL predictors (~.)
summary(lmFitAllPredictors)
lmPred1 <- predict(lmFitAllPredictors, solTestXtrans)
lmValues1 <- data.frame(obs = solTestY, pred = lmPred1)
defaultSummary(lmValues1)
# RMSE = 0.7455802, R2 = 0.8722236, MAE = 0.5497605
observed <- as.vector(solTestY)
predicted <- as.vector(lmPred1)
residualValues <- observed - predicted
summary(residualValues)

axisRange <- extendrange(c(observed, predicted))
par(mfrow = c(1, 2))
plot(predicted, observed, ylim = axisRange, xlim = axisRange)
abline(0, 1, col = "darkgrey", lty = 2)
plot(predicted, residualValues, ylab = "residual")
abline(h = 0, col = "darkgrey", lty = 2)

RMSE(predicted, observed)
R2(predicted, observed)
# Simple correlation
cor(predicted, observed)
# Rank correlation
cor(predicted, observed, method = "spearman")

library(MASS)

rlmFitAllPredictors <- rlm(Solubility ~., data = trainingData)
summary(rlmFitAllPredictors)
rlmPred1 <- predict(rlmFitAllPredictors, solTestXtrans)
rlmValues1 <- data.frame(obs = solTestY, pred = rlmPred1)
defaultSummary(rlmValues1)
# RMSE = 0.7529670, R2 = 0.8700394, MAE = 0.5371296

# 10-fold cross-validation for model performance:
ctrl <- trainControl(method = "cv", number = 10)
set.seed(100)
lmFit1 <- train(x = solTrainXtrans, y = solTrainY, method = "lm", trControl = ctrl)
lmFit1
# RMSE = 0.7357645, R2 = 0.8693657, MAE = 0.5425735

xyplot(solTestY ~ predict(lmFit1, solTestXtrans),
       type = c("p", "g"),
       xlab = "Predicted",
       ylab = "Observed")
xyplot((solTestY - predict(lmFit1, solTestXtrans)) ~ predict(lmFit1, solTestXtrans),
       type = c("p", "g"),
       xlab = "Predicted",
       ylab = "Residuals")

corThreshold <- .9
tooHigh <- findCorrelation(cor(solTrainXtrans), corThreshold)
corrPred <- names(solTrainXtrans)[tooHigh]
trainXfiltered <- solTrainXtrans[, -tooHigh]
testXfiltered <- solTestXtrans[, -tooHigh]

set.seed(100)
lmFiltered <- train(x = trainXfiltered, y = solTrainY, method = "lm", trControl = ctrl)
lmFiltered
# RMSE = 0.7212021, R2 = 0.8743323, MAE = 0.5427594

set.seed(100)
library(doMC)
registerDoMC(8)
rlmPCA <- train(x = solTrainXtrans, y = solTrainY, method = "rlm", preProcess = "pca", trControl = ctrl)
rlmPCA
# RMSE = 0.7838411, R2 = 0.8548926, MAE = 0.6007619

rlmPCA1 <- predict(rlmPCA, solTestXtrans)
rlmPCAValues1 <- data.frame(obs = solTestY, pred = rlmPCA1)
defaultSummary(rlmPCAValues1)

xyplot(rlmPCAValues1$obs ~ rlmPCAValues1$pred,
       type = c("p", "g"),
       xlab = "Predicted",
       ylab = "Residuals")

# PLCR = PLS + PCA
library(pls)
plsFit <- plsr(Solubility ~., data = trainingData)
summary(plsFit)
plsPred1 <- predict(plsFit, solTestXtrans, ncomp = 200) 
plsValues1 <- data.frame(obs = solTestY, pred = as.vector(plsPred1))
defaultSummary(plsValues1)
# ncomp = 1: RMSE = 1.7427958, R2 = 0.8548926, MAE = 1.4477135 
# ncomp = 200: RMSE = 0.7455798, R2 = 0.8722237, MAE = 0.5497599

indx <- createFolds(solTrainY, returnTrain = TRUE)
# ctrl <- trainControl(method = "cv", index = indx) # stays the same
library(caret)
set.seed(100)
plsTune <- train(x = solTrainXtrans, y = solTrainY, 
                 method = "pls",
                 tuneGrid = expand.grid(ncomp = 1:20),
                 trControl = ctrl)
plsTune
plot(plsTune)

PLS_results <- predict(plsTune, solTestXtrans)
PLSValues2 <- data.frame(obs = solTestY, pred = PLS_results)
defaultSummary(PLSValues2)
# RMSE = 0.7108342, R2 = 0.8831725, MAE = 0.5336384

set.seed(100)
pcrTune <- train(x = solTrainXtrans, y = solTrainY,
                 method = "pcr",
                 tuneGrid = expand.grid(ncomp = 1:35),
                 trControl = ctrl)
pcrTune
plot(pcrTune)
PCR_results <- predict(pcrTune, solTestXtrans)
pcrValues <- data.frame(obs = solTestY, pred = PCR_results)
defaultSummary(pcrValues)
# RMSE = 0.8031872, R2 = 0.8508347, MAE = 0.6159903

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)
xyplot(plsPlotData$RMSE ~ plsPlotData$ncomp,
       groups = plsPlotData$Model,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)")
xyplot(Rsquared ~ ncomp,
       data = plsPlotData,
       groups = Model,
       xlab = "# Components",
       ylab = "R2",
       type = c("o", "g"))
plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))

# Penalized Regression Models

# Ridge-regression models
ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))
set.seed(100)
ridgeTune <- train(x = solTrainXtrans, y = solTrainY,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"))
ridge_results <- predict(ridgeTune, solTestXtrans)
ridgeValues <- data.frame(obs = solTestY, pred = ridge_results)
defaultSummary(ridgeValues)
# RMSE = 0.7193079, R2 = 0.8812286, MAE = 0.5375004

# Elastic net (LASSO)
set.seed(100)
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), fraction = seq(.05, 1, length = 20))
enetTune <- train(x = solTrainXtrans, y = solTrainY,
                        method = "enet",
                        tuneGrid = enetGrid,
                        trControl = ctrl,
                        preProc = c("center", "scale"))
enetTune
plot(enetTune)
enet_results <- predict(enetTune, solTestXtrans)
ridgeValues <- data.frame(obs = solTestY, pred = enet_results)
defaultSummary(ridgeValues)
# RMSE = 0.7048937, R2 = 0.8848740, MAE = 0.5295412