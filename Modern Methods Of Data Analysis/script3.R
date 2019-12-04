x <- 1:12
#a random permutation
sample(x)
#bootstrap resampling -- only if length(x) > 1 !
sample(x, replace = T)

library(AppliedPredictiveModeling)
data(twoClassData)
str(predictors)
str(classes)
set.seed(1)
trainingRows <- createDataPartition(classes, p = .80, list = F)
trainingRows

trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]

testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]

str(trainPredictors)
str(testPredictors)

mytable <- predictors
mytable$Class <- classes
xyplot(PredictorB ~ PredictorA, data = mytable, groups = mytable$Class, aspect = 1, type = c('p','g'))

maxDissim(mytable[1,], mytable, n = 10)

set.seed(1)
repeatedSplits <- createDataPartition(trainClasses, p = .80, times = 3)
str(repeatedSplits)

set.seed(1)
str(createResample(trainClasses, times = 3))

trainPredictors <- as.matrix(trainPredictors)
knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
knnFit

testPredictions <- predict(knnFit, newdata = testPredictors, type = "class")
str(testPredictions)
predtable <- cbind(testPredictors, testClasses)
predtable <- cbind(predtable, testPredictions)
sum(predtable$testClasses == predtable$testPredictions)

data(GermanCredit)
str(GermanCredit)

GermanCredit <- GermanCredit[,-nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest <- GermanCredit[-inTrain, ]

library(kernlab)
set.seed(231)
sigDist <- sigest(Class ~ ., data = GermanCreditTrain, frac = 1)
svmTuneGrid <- data.frame(sigma = as.vector(sigDist)[1], C = 2^(-2:7))

library(doMC)
registerDoMC(8)

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5))
svmFit

plot(svmFit, scales = list(x = list(log = 2)))

predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)

#with Probabilities

set.seed(1056)
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneGrid = svmTuneGrid,
                trControl = trainControl(method = "repeatedcv", 
                                         repeats = 5,
                                         classProbs = TRUE))
svmFit
plot(svmFit, scales = list(x = list(log = 2)))
predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)
