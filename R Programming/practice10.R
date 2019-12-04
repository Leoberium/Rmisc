readata <- read.table(file = "hw6.counts.txt", header = TRUE, sep = " ", check.names = FALSE)
zerows <- apply(readata, 1, function(i) all(i == 0))
readata <- readata[zerows == FALSE, ]

pargene <- readata['XLOC_111636', ]
calcdata <- t(rbind(pargene, colSums(readata) - pargene))
colnames(calcdata) <- c('XLOC_111636', 'otha')
calcdata

ages <- as.integer(substring(text = rownames(calcdata), first = 7))
tissue <- substring(text = rownames(calcdata), first = 1, last = 5)
p <- calcdata[, 1]/(calcdata[, 2]+calcdata[, 1])

predictors <- data.frame(tissue = tissue, ages2 = ages^0.5, ages4 = ages^0.25)
gm <- glm(p ~ tissue + ages2 + ages4 + tissue:ages2 + tissue:ages4, data = predictors, family = "quasibinomial")
summary(gm)
anova(gm,test='Chisq')

ages.brain <- sort(runif(56, 10, 82))
ages.liver <- sort(runif(57, 10, 82))
newdata1 <- data.frame(tissue = rep('brain', 56), ages2 = ages.brain^0.5, ages4 = ages.brain^0.25)
newdata2 <- data.frame(tissue = rep('liver', 57), ages2 = ages.liver^0.5, ages4 = ages.liver^0.25)
pred1 <- predict(object = gm, newdata = newdata1, type = 'response')
pred2 <- predict(object = gm, newdata = newdata2, type = 'response')
plot(ages, p, type = 'p', pch = 19, ylim = c(0, 0.0004))
lines(ages.brain, pred1, col = 'blue', lwd = 2)
lines(ages.liver, pred2, col = 'red', lwd = 2)
