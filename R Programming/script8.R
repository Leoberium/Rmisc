cancer <- rnorm(15, 1.5)
healthy <- 1
hist(cancer)
abline(v = healthy, col = "red", lwd = 5)
table(cancer > healthy)

binom.prob <- function(s, n, p) {
  choose(n, s)*(p^s)*((1-p)^(n-s))
}
binom.prob(5, 15, 0.5)
sum(sapply(0:5, function(i) binom.prob(i, 15, 0.5)))
sum(dbinom(0:5, 15, 0.5))
pbinom(5, 15, 0.5)*2
binom.test(5, 15, 0.5)
binom.test(5, 15, 0.5, alternative = "l")

healthy <- rnorm(15, 1)
ustat <- function(x, y) sum(sapply(x, function(i) sum(i > y)))
ustat(healthy, cancer)
ustat(cancer, healthy)
wilcox.test(cancer, healthy)

mean(cancer) - mean(healthy)
permuteMean <- function(x, y, N) {
  r <- numeric(N)
  t <- c(x, y)
  for (i in 1:N) {
    t <- sample(t)
    r[i] <- mean(t[1:15]) - mean(t[16:30])
  }
  return(r)
}
r <- permuteMean(cancer, healthy, 100000)
hist(r)
abline(v = mean(cancer) - mean(healthy), col = "red")
diffe <- mean(cancer) - mean(healthy)
table(diffe > r)
mean(r >= diffe)
t.test(cancer, healthy)
