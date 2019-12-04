library(caret)
library(klaR)
library(MASS)
library(pROC)
library(randomForest)
library(AppliedPredictiveModeling)
library(doMC)
registerDoMC(8)

set.seed(975)
simulatedTrain <- quadBoundaryFunc(500)
simulatedTest <- quadBoundaryFunc(1000)
head(simulatedTrain)

rfModel <- randomForest(class ~ X1 + X2, data = simulatedTrain, ntree = 2000)

qdaModel <- qda(class ~ X1 + X2, data = simulatedTrain)
qdaTrainPred <- predict(qdaModel, simulatedTrain)
qdaTestPred <- predict(qdaModel, simulatedTest)
simulatedTrain$QDAprob <- qdaTrainPred$posterior[, "Class1"]
simulatedTest$QDAprob <- qdaTestPred$posterior[, "Class1"]

rfTestPred <- predict(rfModel, simulatedTest, type = "prob")
head(rfTestPred)
simulatedTest$RFprob <- rfTestPred[, "Class1"]
simulatedTest$RFclass <- predict(rfModel, simulatedTest)

sensitivity(data = simulatedTest$RFclass, reference = simulatedTest$class,
            positive = "Class1")
specificity(data = simulatedTest$RFclass, reference = simulatedTest$class,
            negative = "Class2")

confusionMatrix(data = simulatedTest$RFclass, reference = simulatedTest$class,
                positive = "Class1")
rocCurve <- roc(response = simulatedTest$class, predictor = simulatedTest$RFprob,
                levels = rev(levels(simulatedTest$class)))
auc(rocCurve)
ci(rocCurve)
plot(rocCurve, legacy.axes = FALSE)

calCurve <- calibration(class ~ RFprob + QDAprob + QDAsigmoid, data = simulatedTest)
calCurve
xyplot(calCurve, auto.key = list(columns = 3))

sigmoidalCal <- glm(relevel(class, ref = "Class2") ~ QDAprob, data = simulatedTrain,
                    family = binomial)
coef(summary(sigmoidalCal))
sigmoidProbs <- predict(sigmoidalCal, newdata = simulatedTest[,"QDAprob", drop = FALSE],
                        type = "response")
simulatedTest$QDAsigmoid <- sigmoidProbs

BayesCal <- NaiveBayes(class ~ QDAprob, data = simulatedTrain, usekernel = TRUE)
BayesProbs <- predict(BayesCal, newdata = simulatedTest[, "QDAprob", drop = FALSE])
simulatedTest$QDABayes <- BayesProbs$posterior[, "Class1"]
head(simulatedTest[, c(5:6, 8, 9)])
calCurve2 <- calibration(class ~ QDAprob + QDABayes + QDAsigmoid, data = simulatedTest)
xyplot(calCurve2)
