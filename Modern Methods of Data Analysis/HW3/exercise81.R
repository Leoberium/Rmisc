library(mlbench)
library(caret)
library(doMC)
library(randomForest)
registerDoMC(8)

set.seed(200)
simulated <- mlbench.friedman1(200, sd = 1)
simulated <- cbind(simulated$x, simulated$y)
simulated <- as.data.frame(simulated)
colnames(simulated)[ncol(simulated)] <- "y"

# a
model1 <- randomForest(y ~ ., data = simulated, importance = TRUE, ntree = 1000)
rfImp1 <- varImp(model1, scale = FALSE)
rfImp1

# b
simulated$duplicate1 <- simulated$V1 + rnorm(200) * .1
cor(simulated$duplicate1, simulated$V1)

model2 <- randomForest(y ~ ., data = simulated, importance = TRUE, ntree = 1000)
rfImp2 <- varImp(model2, scale = FALSE)
rfImp2

simulated$duplicate2 <- simulated$V1 + rnorm(200) * .1
cor(simulated$duplicate2, simulated$V1)
model3 <- randomForest(y ~ ., data = simulated, importance = TRUE, ntree = 1000)
rfImp3 <- varImp(model3, scale = FALSE)
rfImp3

# c
library(partykit)
modelC <- cforest(y ~ ., data = simulated[1:11], ntree = 1000)
CImpc <- varimp(object = modelC, conditional = TRUE)
CImpuc <- varimp(object = modelC, conditional = FALSE)
as.data.frame(CImpc)
as.data.frame(CImpuc)

modelCd <- cforest(y ~ ., data = simulated, ntree = 1000)
CdImpc <- varimp(object = modelCd, conditional = TRUE)
CdImpuc <- varimp(object = modelCd, conditional = FALSE)
as.data.frame(CdImpc)
as.data.frame(CdImpuc)

# d
library(gbm)
gbm1 <- gbm(y ~ ., data = simulated[1:11], n.trees = 1000, distribution = 'gaussian')
summary(gbm1)
gbm2 <- gbm(y ~ ., data = simulated, n.trees = 1000, distribution = 'gaussian')
summary(gbm2)

library(Cubist)
cubist1 <- train(y ~ ., data = simulated[1:11], method = 'cubist')
varImp(cubist1)
cubist2 <- train(y ~ ., data = simulated, method = 'cubist')
varImp(cubist2)
