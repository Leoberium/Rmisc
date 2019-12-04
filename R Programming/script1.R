print('Hello, World!')

d = rnorm(300)
plot(d)
d
d[1:10]
summary(d)
hist(d)
den = density(d)
plot(den)
?rnorm
x = seq(-3,3,length.out = 100)
y = dnorm(x)
plot(den,ylim=c(0,max(y,den$y)))
lines(x,y,col='red')
min(d)
mean(d)
median(d)
sd(d)
var(d)

y=rnorm(length(d),mean=d+1,sd=abs(d))
plot(d,y)
d = cbind(d,y)
par(mfrow=c(2,2))
plot(d[,1],d[,2],xlab='original data',ylab='new data')
abline(lm(d[,2] ~ d[,1]),col="red")
boxplot(d)
hist(d[,1])
hist(d[,2])
par(mfrow=c(1,1))
dev.off()
plot(density(d[,2]))
lines(density(d[,1]),col='red')

t.test(d[,1],d[,2])$p.value
wilcox.test(d[,1],d[,2])$p.value

fisher.test(table(first=d[,1] > 0,second=d[,2] > 0))
cor(d[,1],d[,2],method='p')
cor(1:2,2:3,method='sp')
cor.test(d[,1],d[,2],m='sp')$p.value
getwd
getwd()
setwd('R_training/')
getwd()
write.table(d,file='test.tab',sep='\t')
rm(d)
ls
ls()
d=read.table('test.tab')
save.image('all.RData')
rm(list=ls())
ls()
load('all.RData')
saveRDS(d,'d.RDa')
new.d = readRDS('d.RDa')
set.seed(1)
rnorm(1)
pdf('image1.pdf',width=6,height=6)
plot(d)
dev.off
png('image1.png',width=400,height=400)
plot(d)
dev.off
