library(mlbench)
library(caret)
library(doMC)
library(earth)
registerDoMC(8)

set.seed(200)
trainingData <- mlbench.friedman1(200, sd = 1)
trainingData$x <- data.frame(trainingData$x)
featurePlot(trainingData$x, trainingData$y)
testData <- mlbench.friedman1(5000, sd = 1)
testData$x <- data.frame(testData$x)

# Correlation check
corrplot::corrplot(cor(trainingData$x))
findCorrelation(cor(trainingData$x), cutoff = 0.75)
# seems like there are no highly-correlated predictors

# NNET
nnetGrid <- expand.grid(.decay = c(0, 0.01, .1), .size = 1:10, .bag = FALSE)
nnetModel <- train(x = trainingData$x, y = trainingData$y,
                   method = "avNNet", tuneGrid = nnetGrid, 
                   trControl = trainControl(method = "cv"),
                   preProcess = c("center", "scale"),
                   linout = TRUE, trace = TRUE,
                   MaxNWts = 10 * (ncol(trainingData$x) + 1) + 10 + 1,
                   maxit = 500)
nnetModel
plot(nnetModel)

nnetPred <- predict(nnetModel, newdata = testData$x)
modelstats <- postResample(pred = nnetPred, obs = testData$y)


# KNN
knnModel <- train(x = trainingData$x, y = trainingData$y,
                  method = "knn", preProcess = c("center", "scale"),
                  tuneLength = 10)
knnModel
plot(knnModel)

knnPred <- predict(knnModel, newdata = testData$x)
modelstats <- rbind(modelstats, postResample(pred = knnPred, obs = testData$y))

# MARS
marsGrid <- expand.grid(.degree = 1:2, .nprune = 2:38)
marsModel <- train(x = trainingData$x, y = trainingData$y, method = "earth", 
                   tuneGrid = marsGrid, trControl = trainControl(method = "cv"))
marsModel
varImp(marsModel)
plot(marsModel)

marsPred <- predict(marsModel, newdata = testData$x)
modelstats <- rbind(modelstats, postResample(pred = marsPred, obs = testData$y))

# SVMRadial
svmRModel <- train(x = trainingData$x, y = trainingData$y,
                  method = "svmRadial", preProcess = c("center", "scale"),
                  tuneLength = 14, trControl = trainControl(method = "cv"))
svmRModel
plot(svmRModel)
svmRModel$finalModel

svmRPred <- predict(svmRModel, newdata = testData$x)
modelstats <- rbind(modelstats, postResample(pred = svmRPred, obs = testData$y))

# Model comparison
rownames(modelstats) <- c('NNET', 'KNN', 'MARS', 'SVM')
modelstats

