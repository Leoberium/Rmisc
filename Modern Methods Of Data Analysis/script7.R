# Decision trees
library(caret)
library(doMC)
library(rpart)
library(AppliedPredictiveModeling)
registerDoMC(cores = 8)

data(solubility)
trainData <- solTrainXtrans
trainData$y <- solTrainY

# CART method
# makes splits based on the CART methodology

split1 <- rpart(y ~ ., data = trainData, control = rpart.control(maxdepth = 5))
split1

set.seed(100)
indx <- createFolds(solTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
cartTune <- train(x = solTrainXtrans, y = solTrainY,
                  method = "rpart",
                  tuneLength = 25,
                  trControl = ctrl)
cartTune
plot(cartTune)

install.packages("partykit")
library(partykit)
cartTree <- as.party(cartTune$finalModel)
plot(cartTree)

cartImp <- varImp(cartTune, scale = FALSE, competes = FALSE)
cartImp

testResults <- data.frame(obs = solTestY, pred = predict(cartTune, solTestXtrans))
testResults
defaultSummary(testResults)

# unnecessary because of JAVA
install.packages("RWeka")
library(RWeka)
set.seed(100)
indx <- createFolds(solTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
m5Tune <- train(x = solTrainXtrans, y = solTrainY, method = "M5", 
                trControl = ctrl, control = Weka_control(M = 10))

# Tree Bagging
set.seed(100)
treebagTune <- train(x = solTrainXtrans, y = solTrainY,
                     method = "treebag", nbagg = 50, trControl = ctrl)
treebagTune

# Random Forests
mtryGrid <- data.frame(mtry = floor(seq(10, ncol(solTestXtrans), length = 10)))
set.seed(100)
rfTune <- train(x = solTrainXtrans, y = solTrainY, method = "rf",
                tuneGrid = mtryGrid, ntree = 12, importance = TRUE, trControl = ctrl)
rfTune
plot(rfTune)
rfImp <- varImp(rfTune, scale = FALSE)
rfImp

# Boosting
gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                       n.trees = seq(100, 200, by = 10),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 1)
set.seed(100)
gbmTune <- train(x = solTrainXtrans, y = solTrainY, method = "gbm",
                 tuneGrid = gbmGrid, trControl = ctrl, verbose = FALSE)
gbmTune
plot(gbmTune)
defaultSummary(data.frame(obs = solTestY, pred = predict(gbmTune, solTestX)))
