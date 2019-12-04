sapply(split(mtcars$disp, mtcars$cyl), function(x) c(mean = mean(x), sd = sd(x)))

par(fig = c(0, 0.8, 0, 0.8), oma = c(0, 0, 0, 0), mar = c(2, 2, 0, 0))
x <- rnorm(100)
y <- rnorm(100,x)
plot(x,y, pch = 19)

par(fig = c(0.05, 0.4, 0.5, 0.8), new = TRUE)
plot(density(x), xaxt = 'n', yaxt = 'n', main = '', ylab = '', xlab = '')

par(fig = c(0.8, 1, 0, 0.8), new = TRUE)
boxplot(x, horizontal = FALSE)

dev.off()

boxplot(lapply(1:4, function(i) rnorm(100, i)),
        outline = FALSE, notch = TRUE,
        col = heat.colors(4), ylim = c(0, 10))
boxplot(lapply(1:4, function(i) rnorm(100, i + 3)),
        outline = FALSE, notch = TRUE,
        col = heat.colors(4), ylim = c(0, 1), add = TRUE)

x <- c(rnorm(1000, 0), rnorm(1000, 3))
y <- c(rnorm(1000, 0), rnorm(1000, 4))
br <- seq(-4, 7.5, by = 0.1)
h <- hist(x, breaks = br)
h <- hist(y, breaks = br, add = TRUE, density = 30, col = 'black', border = NA)
length(h$breaks)
length(h$counts)
plot(h)
dev.off()

d <- density(x, bw = 0.1, from = -3.8, to = 7.1)
h <- hist(x, breaks = br, freq = TRUE)
d$y <- d$y*length(x)*0.1
lines(d, col = 'red')

m <- round(x)
f <- rep(c('g1', 'g2'), each = 1000)
t <- table(m, f)
barplot(t, beside = T, col = rainbow(nrow(t)))

pie(t[, 1])

x <- round(rnorm(1e4))
y <- round(rnorm(1e4, x))
t <- table(x, y)
image(t, col = rev(heat.colors(100)))
image(log(t+1), col = rev(heat.colors(100)))
image(x = 1:nrow(t), y = 1:ncol(t), z = t,
      col = c('gray', 'yellow', 'red'),
      breaks = c(0, 1, 15, 10000))
x <- seq(0, 5, length.out = 100)
y <- 1 - exp(-x) + rnorm(100, sd = 0.1)
plot(x, y)
lines(x, smooth(y), col = 'red', lwd = 2)
m <- smooth.spline(x, y, df = 6)
p <- predict(m)
lines(m, col = 'green', lwd = 2)

x2 <- x^2
x3 <- x^3
lm <- lm(y ~ x + x2 + x3)
lp <- predict(lm)
lines(x, lp, col = 'blue', lwd = 2)

x <- 1:5
y <- x^4
plot(x, y)
m <- smooth.spline(x, y, df = 3)
p <- predict(m, seq(1, 5, length.out = 100))
lines(p, col = 'green', lwd = 2)

c <- cor(mtcars)
o <- order(c[1,])
image(c[o,o])
heatmap(1-c, symm = T, distfun = function(x) as.dist(x))

pairs(mtcars[,1:3], pch = 19, col = 'red', panel = function(x, y, ...) {
  points(x, y, ...)
  abline(lm(y ~ x), col = 'blue')
})

par(mar = c(4, 4, 4, 4))
a <- c(0, 0.5, 0.7, 5, 10, 50)
h <- rnorm(length(a), 50 + a*2.5)
r <- rnorm(length(a), 5 + a/40)
at <- c(0, 1, 4, 10, 50)
plot(log(a + 1), h, xaxt = 'n')
axis(side = 1, at = log(at + 1), labels = paste0(at, 'y'))
points(log(a + 1), r/max(r)*max(h), col = 'red')
at2 <- 4:6
axis(side = 4, at = at2/max(r)*max(h), at2, col = "red")
mtext('redius', 4, 3)
