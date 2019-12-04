x <- runif(1,0,3)
icol <- 1:15
t.app <- numeric(length(icol))
t.app[1] <- x
for(i in 2:length(icol)){
  t.app[i] <- t.app[i-1] + (-1)^(i+1)*x^(2*i-1)/factorial(2*i-1)
}
difference <- abs(t.app - sin(x))
t.app2 <- x
par(mfrow=c(1,2))
plot(1:length(icol),difference,t='l',log='y')

y <- runif(1000000,-10,10)
system.time(sort(y))
limit <- 25

difference2 <- numeric(100)
for(i in 1:100){
  t.app2 <- y[i]
  for(k in 2:limit){
    t.app2 <- t.app2 + (-1)^(k+1)*y[i]^(2*k-1)/factorial(2*k-1)
    difference2[i] <- abs(sin(y[i])-t.app2)
  }
}
#o <- order(y)
#plot(y[o],difference2[o],t='l')
plot(y,difference2)

