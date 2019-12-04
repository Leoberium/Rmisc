factorialcustom <- function(n) {
  if (n == 1) return(1)
  else return(n*factorialcustom(n-1))
}

factorialcustom(5)

harmonicmean <- function(x) {
  return(nrow(x)/colSums(1/x))
}

harmonicmean(mtcars)

calcPolynomial <- function(x, coefs){
  return(coefs[1] + sum(cumprod(rep(x, length(coefs) - 1))*coefs[2:length(coefs)]))
}

calcPolynomial(c(1,9), c(1, 0, 2))


p <- getPolynomial <- function(coefs) {
  function(x) coefs[1] + sum(cumprod(rep(x, length(coefs) - 1))*coefs[2:length(coefs)])
}

p(1:10)(2)


