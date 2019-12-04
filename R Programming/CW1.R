integroid <- function(FUN = function(x) exp(x), from = 0, to = 1,
                      diffint = 1e-10, max.iter = 1e4) {
  results <- numeric(max.iter)
  for (i in 1:2) {
    step <- (to - from)/(i+1)
    indices <- seq(from, to, by = step)
    endt <- length(indices)
    results[i] <- sum(step*(FUN(indices[-1]) + FUN(indices[-endt]))/2)
  }
  while (abs(results[i]-results[i-1]) >= diffint && i < max.iter) {
    i <- i + 1
    step <- (to - from)/(i+1)
    indices <- seq(from, to, by = step)
    endt <- length(indices)
    results[i] <- sum(step*(FUN(indices[-1]) + FUN(indices[-endt]))/2)
  }
  return(results[i])
}
integroid(function(x) {log2(x)}, from = 1, to = 1000, 1e-10, 1e4)

