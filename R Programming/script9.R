d <- matrix(rnorm(1e5), ncol = 10)
d[1:500, 6:10] <- rnorm(2500, 3)

d1 <- matrix(rnorm(1e5), ncol = 10)
d1[1:500, 6:10] <- rnorm(2500, 3)

testDE <- function(e) {
  pv <- apply(e, 1, function(x) t.test(x[1:5], x[6:10])$p.value)
  d1ff <- apply(e, 1, function(x) mean(x[1:5] - x[6:10]))
  data.frame(pv = pv, d1ff = d1ff, adj.pv = p.adjust(pv, method = "BH"))
}

r <- testDE(d)
table(r$adj.pv<0.05)
hist(r$d1ff[r$adj.pv<0.05], 50)
abline(v = -3, col = "red")
hist(r$d1ff[1:500], 50)

r1 <- testDE(d1)
hist(r1$d1ff[r$adj.pv<0.05], 50)

plot(r$d1ff[r$adj.pv<0.05], r1$d1ff[r$adj.pv<0.05])
plot(r$d1ff[r$adj.pv<0.05]/r1$d1ff[r$adj.pv<0.05], ylim = c(0, 2))
abline(h = 1, col = "red")

table(r$adj.pv<0.05, r1$adj.pv<0.05)

pca <- prcomp(t(d))
dim(pca$x)
var <- round(pca$sdev/sum(pca$sdev)*100, 1)
plot(pca$x, col = rep(c("red", "blue"), each = 5), pch = 19,
     xlab = paste0('PC1 (', var[1], '%)'))

cord <- cor(d, m = "sp")
dim(cord)
cord <- cor(d, m = "sp", use = "p")
image(cord)
heatmap(cord, symm = TRUE, RowSideColors = rep(c("red", "blue"), each = 5),
        distfun = function(x) as.dist(1-x))

mds <- cmdscale(1-cord, k = 2)
plot(mds, col = rep(c("red", "blue"), each = 5), pch = 19)

t <- t.test(d[1, 1:5], d[1, 6:10], a="less", var.equal = TRUE)
t

m <- matrix(c(100, 98, 500, 300), ncol = 2)
fisher.test(m)

ma <- matrix(c(511, 314, 89, 19), ncol = 2)
ph <- matrix(c(138, 279, 131, 244), ncol = 2)
fisher.test(ma + ph)
fisher.test(ma)
fisher.test(ph)

?rpois
