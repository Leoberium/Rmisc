y <- rpois(2000, 100)
(yhist <- as.vector(table(cut(y, breaks = 5:15*10))))
(yhist2 <- table(ceiling(y/10)*10))
par(mfrow = c(1, 1))
ins <- 1 - yhist/max(yhist)
colorsCol <- rgb(ins, ins, ins)
modey <- names(sort(table(y), decreasing = TRUE))[1]
plot(5:14*10, yhist, t = 'h', xlim = c(50, 150), lwd = 5, lty = 1,
     xlab = 'value', ylab = 'quantity', col = colorsCol)
abline(v = c(mean(y), median(y), modey), 
      col = c('red', 'blue', 'green'), lwd = 2)

