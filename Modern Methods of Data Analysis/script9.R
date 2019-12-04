library(caret)
library(doMC)
library(glmnet)
library(MASS)
library(pROC)
registerDoMC(8)

load('training.RData')
load('testing.RData')
str(training)
str(testing)
load('pre2008Data.RData')
load('year2008Data.RData')
str(pre2008Data)
str(year2008Data)

fullSet <- names(training)[names(training) != "Class"]
predCorr <- cor(training[, fullSet])
highCorr <- findCorrelation(predCorr, .99)
fullSet <- fullSet[-highCorr]

isNZV <- nearZeroVar(training[, fullSet],
                     saveMetrics = TRUE, freqCut = floor(nrow(training)/5))
fullSet <- rownames(subset(isNZV, !nzv))
str(fullSet)

reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(training)/50)))
reducedSet <- reducedSet[(reducedSet!= "allPub") &
                           (reducedSet != "numPeople") &
                           (reducedSet != "Mar") &
                           (reducedSet != "Sun")
                         ]
str(reducedSet)

# Logistic Regression
ctrl <- trainControl(summaryFunction = twoClassSummary, classProbs = TRUE,
                     savePredictions = TRUE)
set.seed(476)
lrReduced <- train(training[, reducedSet], y = training$Class,
                   method = "glm", metric = "ROC", trControl = ctrl)
lrReduced
head(lrReduced$pred)

lrTestClasses <- predict(lrReduced, newdata = testing[, reducedSet])
lrTestProbs <- predict(lrReduced, newdata = testing[, reducedSet],
                       type = "prob")
confusionMatrix(data = lrTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc <- roc(response = testing$Class,
                  predictor = lrTestProbs$successful,
                  levels = rev(levels(testing$Class)))
plot(reducedRoc, legacy.axes = TRUE)
auc(reducedRoc)
modelstats <- c(0.908, 0.838, 0.783, 0.860)
names(modelstats) <- c("AUC", "Accuracy", "Sensitivity", "Specificity")
# confusionMatrix(data = lrReduced$pred$pred,
#                 reference = lrReduced$pred$obs)
# reducedRoc <- roc(response = lrReduced$pred$obs,
#                   predictor = lrReduced$pred$successful,
#                   levels = rev(levels(lrReduced$pred$obs)))
# plot(reducedRoc, legacy.axes = TRUE)
# auc(reducedRoc)

# Linear Discriminant Analysis
set.seed(476)
ldaFit <- train(x = training[, reducedSet], y = training$Class,
                method = "lda", preProcess = c("center", "scale"),
                metric = "ROC",
                trControl = ctrl)
ldaFit
head(ldaFit$pred)

ldaTestClasses <- predict(ldaFit, newdata = testing[, reducedSet])
ldaTestProbs <- predict(ldaFit, newdata = testing[, reducedSet],
                       type = "prob")
confusionMatrix(data = ldaTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc2 <- roc(response = testing$Class,
                  predictor = ldaTestProbs$successful,
                  levels = rev(levels(testing$Class)))
plot(reducedRoc2, legacy.axes = TRUE)
auc(reducedRoc2)
modelstats <- rbind(modelstats, c(0.921, 0.849, 0.825, 0.863))
rownames(modelstats) <- c("LR", "LDA")

# Partial Least Squares Discriminant Analysis
set.seed(476)
plsFit <- train(x = training[, reducedSet], y = training$Class,
                method = "pls", tuneGrid = expand.grid(.ncomp = 1:10),
                preProcess = c("center", "scale"),
                metric = "ROC",
                trControl = ctrl)
plsFit
head(plsFit$pred)

plsTestClasses <- predict(plsFit, newdata = testing[, reducedSet])
plsTestProbs <- predict(plsFit, newdata = testing[, reducedSet],
                        type = "prob")
confusionMatrix(data = plsTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc3 <- roc(response = testing$Class,
                   predictor = plsTestProbs$successful,
                   levels = rev(levels(testing$Class)))
plot(reducedRoc3, legacy.axes = TRUE)
auc(reducedRoc3)
modelstats <- rbind(modelstats, c(0.921, 0.849, 0.841, 0.854))
rownames(modelstats) <- c("LR", "LDA", "PLS")
plot(plsFit)
plsImpGrant <- varImp(plsFit, scale = TRUE)
plot(plsImpGrant)

# Penalized Models
glmnGrid <- expand.grid(alpha = c(0, .1, .2, .4, .6, .8, 1),
                        lambda = seq(.01, .2, length = 40))
glmnTuned <- train(training[, fullSet], y = training$Class,
                   method = "glmnet", tuneGrid = glmnGrid,
                   preProcess = c("center", "scale"),
                   metric = "ROC",
                   trControl = ctrl)
plot(glmnTuned, plotType = "level")
glmnTuned
head(glmnTuned$pred)

glmnTestClasses <- predict(glmnTuned, newdata = testing[, fullSet])
glmnTestProbs <- predict(glmnTuned, newdata = testing[, fullSet],
                        type = "prob")
confusionMatrix(data = glmnTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc4 <- roc(response = testing$Class,
                   predictor = glmnTestProbs$successful,
                   levels = rev(levels(testing$Class)))
plot(reducedRoc4, legacy.axes = TRUE)
auc(reducedRoc4)
modelstats <- rbind(modelstats, c(0.931, 0.857, 0.873, 0.848))
rownames(modelstats) <- c("LR", "LDA", "PLS", "GLMN")

modelstats

# Continuing with non-linear models
# Mixture discriminant analysis
load('mdaFit.RData')
plot(mdaFit)
mdaFit

mdaTestClasses <- predict(mdaFit, newdata = testing[, fullSet])
mdaTestProbs <- predict(mdaFit, newdata = testing[, fullSet],
                         type = "prob")
confusionMatrix(data = mdaTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc5 <- roc(response = testing$Class,
                   predictor = mdaTestProbs$successful,
                   levels = rev(levels(testing$Class)))
plot(reducedRoc5, legacy.axes = TRUE)
auc(reducedRoc5)
modelstats <- rbind(modelstats, c(0.921, 0.849, 0.825, 0.863))
rownames(modelstats) <- c("LR", "LDA", "PLS", "GLMN", "MDA")

modelstats

# Neural network
load('nnetFit.RData')
plot(nnetFit)
nnetFit

nnetTestClasses <- predict(nnetFit, newdata = testing[, fullSet])
nnetTestProbs <- predict(nnetFit, newdata = testing[, fullSet],
                        type = "prob")
confusionMatrix(data = nnetTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc6 <- roc(response = testing$Class,
                   predictor = nnetTestProbs$successful,
                   levels = rev(levels(testing$Class)))
plot(reducedRoc6, legacy.axes = TRUE)
auc(reducedRoc6)
modelstats <- rbind(modelstats, c(0.919, 0.842, 0.820, 0.854))
rownames(modelstats) <- c("LR", "LDA", "PLS", "GLMN", "MDA", "NNET")

modelstats

# SVM
load('svmRModel.RData')
svmRModel # doesn't work further


# KNN
load('knnFit.RData')
knnFit

knnTestClasses <- predict(knnFit, newdata = testing[, fullSet])
knnTestProbs <- predict(knnFit, newdata = testing[, fullSet],
                         type = "prob")
confusionMatrix(data = knnTestClasses, reference = testing$Class,
                positive = "successful")
reducedRoc7 <- roc(response = testing$Class,
                   predictor = knnTestProbs$successful,
                   levels = rev(levels(testing$Class)))
plot(reducedRoc7, legacy.axes = TRUE)
auc(reducedRoc7)
modelstats <- rbind(modelstats, c(0.830, 0.697, 0.307, 0.921))
rownames(modelstats) <- c("LR", "LDA", "PLS", "GLMN", "MDA", "NNET", "KNN")

modelstats

# NaiveBayes
load('nbPredictors.RData')
load('nbTesting.RData')
load('nbTraining.RData')

load('nBayesFit.RData')
nBayesFit
