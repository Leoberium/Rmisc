#Лев Мазаев
#10000 равномерно распределенных точек
dots.quantity <- 10000
x.cds <- runif(dots.quantity,-1,1)
y.cds <- runif(dots.quantity,-1,1)

#вероятность через цикл
counter <- 0
for(i in 1:dots.quantity){
  counter <- counter + ((x.cds[i]**2 + y.cds[i]**2)**(1/2) < 1)
}
prob.cycle <- counter/dots.quantity

#вероятность без цикла
prob.wo.cycle <- mean((c((x.cds**2 + y.cds**2)**(1/2) < 1)))

#посчитанные в цикле и без цикла вероятности равны
prob.cycle == prob.wo.cycle
prob.cycle
prob.wo.cycle

#посчитанное оценка pi
pi.eval <- prob.cycle*4
pi.eval

#зависимость оценки pi от числа бросков
#используется та же симуляция в 10000 точек
#оценка числа pi считается с шагом в 10 точек
step <- 10
pi.calc <- numeric(dots.quantity/step)
for(i in 1:dots.quantity/step){
  pi.calc[i] <- 4*sum(c((x.cds[1:(step*i)]**2 + y.cds[1:(step*i)]**2)**(1/2) < 1))/(step*i)
}
par(mfrow=c(1,2))
plot((1:(dots.quantity/step))*step,pi.calc,
     type='l',
     xlab='Число бросков',
     ylab='Оценка числа pi',
     main='Оценка числа pi в зависимости от числа бросков'
     )
abline(h=pi,col='red',lwd='2')
plot((1:(dots.quantity/step))*step,abs(pi.calc-pi),
     type='l',
     log='y',
     xlab='Число бросков',
     ylab='Abs(pi-pi.calc)',
     main='Abs(pi-pi.calc) в зависимости от числа бросков'
     )

