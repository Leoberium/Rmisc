library(caret)
library(corrplot)
library(doMC)
registerDoMC(8)
data(tecator)
?tecator

splom(~endpoints)
any(is.na(absorp))
any(is.na(endpoints))
colnames(absorp) <- paste0("V", 1:100)

# plot 10 random spectra 
p10randspectra <- function() {
set.seed(1)
inSubset <- sample(1:dim(endpoints)[1], 10)

absorpSubset <- absorp[inSubset,]
endpointSubset <- endpoints[inSubset, 3]

newOrder <- order(absorpSubset[,1])
absorpSubset <- absorpSubset[newOrder,]
endpointSubset <- endpointSubset[newOrder]

plotColors <- rainbow(10)

plot(absorpSubset[1,],
     type = "n",
     ylim = range(absorpSubset),
     xlim = c(0, 105),
     xlab = "Wavelength Index",
     ylab = "Absorption")

for(i in 1:10)
{
  points(absorpSubset[i,], type = "l", col = plotColors[i], lwd = 2)
  text(105, absorpSubset[i,100], endpointSubset[i], col = plotColors[i])
}
title("Predictor Profiles for 10 Random Samples")
}
p10randspectra()

# correlation matrix
corrplot(corr = cor(absorp), order = "hclust")
# all predictors are highly correlated

# PCA
pcaObject <- prcomp(absorp, center = TRUE, scale. = TRUE)
summary(pcaObject)
# the effective dimension of the data is 1 because PC1 catches 98.6% of variance
plot(1:100, summary(pcaObject)$importance[2, ], type = "b",
     pch = 19, xlab = "# of component", ylab = "proportion of variance")
rm(pcaObject)
# PCA in preProcess
PCAFit <- preProcess(absorp, method = c("pca"))
PCAFit
rm(PCAFit)
# this PCA model uses 2 components

# splitting and pre-processing the data 
trainrows <- createDataPartition(1:215, p = 0.6, list = TRUE)[[1]]
transFit <- preProcess(absorp, method = c("BoxCox", "center", "scale"))
transAbsorp <- as.data.frame(predict(transFit, absorp))
trainX <- transAbsorp[trainrows, ]
testX <- transAbsorp[-trainrows, ]

# transFit <- preProcess(as.data.frame(endpoints), method = c("center", "scale"))
# transEndpoints <- as.data.frame(predict(transFit, as.data.frame(endpoints)))
transEndpoints <- as.data.frame(endpoints)
trainY <- transEndpoints[trainrows, 2] # col #2 because only fat will be predicted
testY <- transEndpoints[-trainrows, 2]
trainingData <- cbind(trainX, trainY)
colnames(trainingData)[101] <- "FatP"

# visualizing
ggplot(trainingData, aes(x = V1, y = FatP)) +
  geom_point()

modeval <- function(model, X = testX, Y = testY) {
  pred <- predict(model, X)
  values <- data.frame(obs = Y, pred = pred)
  return(defaultSummary(values))
}

# Ordinary Linear Regression, lm function
lmbasic <- lm(FatP ~ ., data = trainingData)
summary(lmbasic)
modeval(lmbasic)
modstats <- t(as.data.frame(modeval(lmbasic)))
rownames(modstats)[1] <- "LM basic R"
plot(testY, predict(lmbasic, testX), pch = 19)

# Ordinary Linear Regression, lm from caret
ctrl <- trainControl(method = "cv", number = 10)
lmFit <- train(x = trainX, y = trainY, method = "lm", trControl = ctrl)
lmFit
modeval(lmFit)
modstats <- rbind(modstats, modeval(lmFit))
rownames(modstats)[2] <- "LM caret"
plot(testY, predict(lmFit, testX), pch = 19)

# Ordinary Linear Regression with filtering highly correlated values
tooHigh <- findCorrelation(cor(trainX), cutoff = 0.99)
length(tooHigh)
trainXfiltered <- trainX[, -tooHigh]
testXfiltered <- testX[, -tooHigh]
lmFiltered <- train(x = trainXfiltered, y = trainY,
                    method = "lm", trControl = ctrl)
lmFiltered
modeval(lmFiltered, X = testXfiltered)
modstats <- rbind(modstats, modeval(lmFiltered, X = testXfiltered))
rownames(modstats)[3] <- "LM filtered"
plot(testY, predict(lmFiltered, testXfiltered), pch = 19)

# Robust Linear Regression, rlm from MASS
library(MASS)
rlmFit <- rlm(FatP ~ ., data = trainingData)
summary(rlmFit)
modeval(rlmFit)
modstats <- rbind(modstats, modeval(rlmFit))
rownames(modstats)[4] <- "RLM MASS"
plot(testY, predict(rlmFit, testX), pch = 19)

# Robust Linear Regression with PCA
rlmPCA <- train(x = trainX, y = trainY, method = "rlm", preProcess = "pca",
                trControl = ctrl)
summary(rlmPCA)
modeval(rlmPCA)
modstats <- rbind(modstats, modeval(rlmPCA))
rownames(modstats)[5] <- "RLM PCA"
plot(testY, predict(rlmPCA, testX), pch = 19)

# Partial Least Squares Regression
indx <- createFolds(trainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
plsTune <- train(x = trainX, y = trainY, method = "pls", 
                 tuneGrid = expand.grid(ncomp = 1:50),
                 trControl = ctrl)
plsTune
plot(plsTune)
modeval(plsTune)
modstats <- rbind(modstats, modeval(plsTune))
rownames(modstats)[6] <- "PLSR"
plot(testY, predict(plsTune, testX), pch = 19)

# Principal Component Regression
pcrTune <- train(x = trainX, y = trainY, method = "pcr", 
                 tuneGrid = expand.grid(ncomp = 1:50),
                 trControl = ctrl)
pcrTune
plot(pcrTune)
modeval(pcrTune)
modstats <- rbind(modstats, modeval(pcrTune))
rownames(modstats)[7] <- "PCR"
plot(testY, predict(pcrTune, testX), pch = 19)

# Ridge Regression
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))
ridgeRegFit <- train(x = trainX, y = trainY, method = "ridge", tuneGrid = ridgeGrid,
                     trControl = ctrl)
ridgeRegFit
plot(ridgeRegFit)
modeval(ridgeRegFit)
modstats <- rbind(modstats, modeval(ridgeRegFit))
rownames(modstats)[8] <- "Ridge"
plot(testY, predict(ridgeRegFit, testX), pch = 19)

# LASSO Regression
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(0.5, 1, length = 20))
enetTune <- train(x = trainX, y = trainY, method = "enet", tuneGrid = enetGrid,
                  trControl = ctrl)
enetTune
plot(enetTune)
modeval(enetTune)
modstats <- rbind(modstats, modeval(enetTune))
rownames(modstats)[9] <- "LASSO"
plot(testY, predict(enetTune, testX), pch = 19)

modstats

