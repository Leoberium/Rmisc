getwd()
setwd("R_training/")
genexpr <- read.table("hw1.txt")

#Мазаев Лев
#Значимая зависимость в уровне экспрессии есть только между CPY14 и RMD4
#Есть значимые различия между средними уровнями экспрессии в случае пары CPY14 и RMD4, а также RMD4 и htVWQ

#Пара CPY14 - RMD4
cor.test(genexpr$CPY14,genexpr$RMD4)
cor.test(genexpr$CPY14,genexpr$RMD4)$p.value
#Есть значимая зависимость, так как p < 0.05

t.test(genexpr$CPY14,genexpr$RMD4)
t.test(genexpr$CPY14,genexpr$RMD4)$p.value
#Есть значимое отличие между средними, так как p < 0.05

#Пара CPY14 - htVWQ
cor.test(genexpr$CPY14,genexpr$htVWQ)
cor.test(genexpr$CPY14,genexpr$htVWQ)$p.value
#Нет значимой зависимости, так как p > 0.05

t.test(genexpr$CPY14,genexpr$htVWQ)
t.test(genexpr$CPY14,genexpr$htVWQ)$p.value
#Нет значимых отличий среднего, так как p > 0.05

#Пара RMD4 - htVWQ
cor.test(genexpr$RMD4,genexpr$htVWQ)
cor.test(genexpr$RMD4,genexpr$htVWQ)$p.value
#Нет значимой зависимости, так как p > 0.05

t.test(genexpr$RMD4,genexpr$htVWQ)
t.test(genexpr$RMD4,genexpr$htVWQ)$p.value
#Есть значимое отличие между средними, так как p < 0.05

#Графики
par(mfrow = c(2,2))
boxplot(genexpr$CPY14,genexpr$RMD4,genexpr$htVWQ,col=c("red","blue","green"),names=c("CPY14","RMD4","htVWQ"),main="Genes Expression")
plot(genexpr$CPY14,genexpr$RMD4,xlab="CPY14",ylab="RMD4",main="RMD4-CPY14 comparison")
abline(lm(genexpr$RMD4 ~ genexpr$CPY14),col="red")
plot(genexpr$CPY14,genexpr$htVWQ,xlab="CPY14",ylab="htVWQ",main="htVWQ-CPY14 comparison")
abline(lm(genexpr$htVWQ ~ genexpr$CPY14),col="red")
plot(genexpr$RMD4,genexpr$htVWQ,xlab="RMD4",ylab="htVWQ",main="htVWQ-RMD4 comparison")
abline(lm(genexpr$htVWQ ~ genexpr$RMD4),col="red")