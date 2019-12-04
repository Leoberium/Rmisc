l = list(a = 1:10, b = -1:3, c = 2:301)
lapply(l, mean)
sapply(l, mean)
sapply(l, summary)
t(sapply(l, summary))
l$b[2] <- NA
sapply(l, mean)
mean(l$b, na.rm = T) # remove NA

sapply(l, mean, na.rm = T)
sapply(l, mean, na.rm = T, trim = 0.1)
sapply(1:10, max)
sapply(l, '[', 1) # first from each

sapply(l, function(x) {x[seq(1, by = 3, to = length(x))]})
sapply(l, '[', c(T, F, F))

sapply(l, function(x) prod(abs(x), na.rm = T)^(1/length(x)))

m = matrix(rnorm(100, 10 + rep(1:10, each = 10)), ncol = 10)
apply(m, 2, mean)
nf = apply(m, 2, max)

sweep(m, 2, nf, '/')
?scale
apply(m, 1:2, '^', 2)

do.call(plot, list(x = 1:10, y = sin(1:10)))
apply(round(m), 1, paste, collapse = ",")
t <- apply(round(m), 1, paste, collapse = ",")
do.call(rbind, lapply(strsplit(t, ","), as.numeric))

ifelse(runif(10) > 0.5, 's', 'f')        

sex <- ifelse(runif(20) > 0.5, 'm', 'f')
height <- ifelse(sex == 'm', rnorm(20, 182), rnorm(20, 172))
height[sex == "m"]
split(height, sex)
sh <- split(height, sex)
boxplot(height ~ sex)
sapply(sh, mean)

data = data.frame(sex, height, age = rnorm(length(sex), 35, 5))
t <- split(data, data$sex)
sapply(t, function(x) x[which.max(x$height), ])
do.call(rbind, lapply(t, function(x) x[which.max(x$height), ]))

split.data.frame(as.matrix(data), data$sex)

install.packages("plyr")
library(plyr)
baseball
laply(baseball, typeof, .progress = 'text')

library(doMC)
registerDoMC(8)

system.time(laply(1:10, function(i) mean(rnorm(5e6)), .parallel = TRUE))

