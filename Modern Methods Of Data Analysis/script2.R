library(AppliedPredictiveModeling)
data("segmentationOriginal")
setwd('ModernDataAnalysis/')
structure(segmentationOriginal)
summary(segmentationOriginal$Case)
segData <- subset(segmentationOriginal, Case == "Train")
Cell <- (segData$Cell)
Class <- segData$Class
Case <- segData$Case
segData <- segData[,-(1:3)]
statusColNum <- grep("Status", names(segData))
segData <- segData[,-statusColNum]
library(e1071)
skewness(segData$AngleCh1)
skewValues <- apply(segData, 2, skewness)
skewValues
head(skewValues)
library(caret)
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans
head(segData$AreaCh1)
predict(Ch1AreaTrans, head(segData$AreaCh1))
hist(predict(Ch1AreaTrans, segData$AreaCh1))
pcaObject <- prcomp(segData,center=TRUE,scale.=TRUE)
pcaObject
percentVariance <- pcaObject$sd^2/sum(pcaObject$sd^2)*100
percentVariance[1:3]
head(pcaObject$x[, 1:5])
head(pcaObject$rotation[, 1:3])

trans <- preProcess(segData,method=c("BoxCox","center","scale","pca"))
trans
transformed <- predict(trans,segData)
head(transformed[,1:5])

segTrain <- subset(segmentationOriginal, Case=="Train")
segTrainX <- segTrain[, -(1:3)]
segTrainClass <- segTrain$Class
segPP <- preProcess(segTrainX,method="BoxCox")
segTrainTrans <- predict(segPP,segTrainX)

xyplot(AvgIntenCh1 ~ EntropyIntenCh1, data=segTrainTrans, groups=segTrain$Class,xlab="Channel 1 Fiber Width",ylab="Intensity Entropy Channel 1",auto.key=list(columns=2),type=c("p","g"),main="Original Data",aspect=1)

pr <- prcomp(~ AvgIntenCh1 + EntropyIntenCh1,data=segTrainTrans,scale. = TRUE)
xyplot(PC2 ~ PC1,
       data = as.data.frame(pr$x),
       groups = segTrain$Class,
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Transformed",
       xlim = extendrange(pr$x),
       ylim = extendrange(pr$x),
       type = c("p", "g"),
       aspect = 1)

xyplot(PC2 ~ PC1,
       data = as.data.frame(transformed),
       groups = segTrain$Class,
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Transformed",
       xlim = extendrange(transformed$PC1),
       ylim = extendrange(transformed$PC2),
       type = c("p", "g"),
       aspect = 1)

isZV <- apply(segTrainX, 2, function(x) length(unique(x)) == 1)
segTrainX <- segTrainX[, !isZV]
segPP <- preProcess(segTrainX, c("BoxCox", "center", "scale"))
segTrainTrans <- predict(segPP, segTrainX)
segPCA <- prcomp(segTrainTrans, center = TRUE, scale. = TRUE)

transparentTheme(pchSize = .8, trans = .3)
panelRange <- extendrange(segPCA$x[, 1:3])
splom(as.data.frame(segPCA$x[, 1:3]),
      groups = segTrainClass,
      type = c("p", "g"),
      as.table = TRUE,
      auto.key = list(columns = 2),
      prepanel.limits = function(x) panelRange)

nearZeroVar(segData)
correlations <- cor(segData)
dim(correlations)
correlations[1:5, 1:5]

install.packages("corrplot")
library(corrplot)
corrplot(correlations, order = "hclust")

highCorr <- findCorrelation(correlations, cutoff = .75)
length(highCorr)
head(highCorr)
filteredSegData <- segData[, -highCorr]

data(cars)
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x==1)]))
carSubset <- cars[, c(1,2,19)]
head(carSubset)
levels(carSubset$type)

simpleMod <- dummyVars(~Mileage + type, data = carSubset, levelsOnly = T)
simpleMod
newTable <- predict(simpleMod, carSubset)
newTable

withinteraction <- dummyVars(~Mileage + type + Mileage:type,
                             data = carSubset,
                             levelsOnly = T)
withinteraction
newTable1 <- predict(withinteraction, head(carSubset))
newTable1
