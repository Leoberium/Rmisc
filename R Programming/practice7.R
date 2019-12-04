imageWithText <- function(D, digits = 2, ...) {
  Drot <- t(D)[, nrow(D):1]
  Drnd <- round(D, digits = digits)
  x <- 1:ncol(D)
  y <- 1:nrow(D)
  image(x, y, Drot, xaxt = "n", yaxt = "n", ...)
  for (i in 1:2) {
    mtext(colnames(D), 2*i-1, 0.75, cex = 1, at = 1:ncol(D))
    mtext(rownames(D), 2*i, 0.75, cex = 1, at = nrow(D):1)
  }
  for (i in 1:nrow(Drnd)) {
    for (j in 1:ncol(Drnd)) {
      text(j, i, labels = Drnd[nrow(Drnd)+1-i, j])
    }
  }
  return(Drnd)
}

c <-  cor(mtcars)
c
imageWithText(c, col = heat.colors(100))
image(c, col = heat.colors(100))
imageWithText(c, col = gray(50:100/100))
imageWithText(matrix(1:100, ncol = 10), col = gray(1:100/100))
