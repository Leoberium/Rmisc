findns <- function(x, n) {
  sort(x, decreasing = T)[n]
}

findns(c(1:25, -57:-47), 4)

mnorm <- function(m, dimtype, fun, operand, ...) {
  x <- apply(m, dimtype, fun, ...)
  return(sweep(m, MARGIN = dimtype, STATS = x, FUN = operand))
}

mat1 <- matrix(rnorm(140, sd = 25), nrow = 20, ncol = 7)

mnorm(mat1, 2, findns, "/", round((1/4)*nrow(mat1)))
# Another way:
mnorm(mat1, 2, quantile, "/", probs = 0.75)


