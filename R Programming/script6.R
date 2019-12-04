# Ещё реализация подсчета слов длины k
install.packages("seqinr")
library(seqinr)
?count

cw4 <- function(t, l) {
  t <- strsplit(t, '')[[1]]
  i <- 1:(length(t) - l +1)
  table(do.call(paste0, lapply(1:l, function(i) t[i:(length(t-l+i))])))
}

# Graphics

x <- seq(0, 5, length.out = 20)
y <- 1-exp(-x)
y <- rnorm(length(y), y, 0.03)
plot(x, y)

par(mfrow = c(2, 3)) #graphical parameters
plot(x, y, t = 'p')
plot(x, y, t = 'l')
plot(x, y, t = 'b')
plot(x, y, t = 's')
plot(x, y, t = 'S')
plot(x, y, t = 'h')
plot(x, y, log = 'xy')

par(mfrow = c(1, 1))
plot(1, t = 'n', xlim = c(0, 1), ylim = c(0, 6)) #empty plot
for(i in 1:6) lines(0:1, c(i, i), lty = i, lwd = i) #lines
plot(1, t = 'n', xlim = c(0, 1), ylim = c(0, 6)) #new empty plot
for(i in 1:6) lines(0:1, c(i, i), lty = i, lwd = i, t = 'b') #lines with points

x <- rep(1:5, times = 5)
y <- rep(1:5, each = 5)
plot(x, y, pch = 0:24) #point charter
plot(x, y, pch = 0:24, cex = seq(1, 7, length.out = 25)) #point size

colors()
plot(x, y, pch = 0:24, col = 'red')
plot(x, y, pch = 0:24, col = '#0080FFFF')
rgb(0, 1, 1, alpha = 0.5)
col2rgb('red')
heat.colors(10)
rainbow(10)

install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal.info
?brewer.pal.info
display.brewer.pal(8, 'Dark2')
display.brewer.pal(8, 'Blues')
display.brewer.pal(8, 'Reds')
display.brewer.all()
col <-  brewer.pal(8, 'Reds')
plot(1:8, 1:8, col = col[7:8], pch = 19, cex = 3)
plot(1:8, 1:8, col = rainbow(8), pch = 19, cex = 3)

plot(1, t = 'n', xlim =c(0, 100), ylim = c(0,100))
lines(0:100, 90 + sin(0:100/10)*10)
x <-  0:10*10
y <- 90 + cos(0:10)*10
points(x, y, pch = 19) #the same as lines, but with other t parameter
segments(x, y - 3, x, y + 3) #error on plot
text(x, y, paste0('x = ', x), adj = c(-0.1, -0.1)) #text on plot
polygon(c(0, 10, 20), c(0, 20, 0), col = 'blue', border = 'red')
x = seq(0, 2*pi, length.out = 200)
polygon(30 + 10*sin(x), 30 + 20*cos(x), col = 'red', den = 20, angle = -45)

rect(40, 40, 60, 60)
abline(h = 0:5*20, lty = 2, col = 'gray')
abline(a = 0, b = 1, lty = 2)

plot(1)
grid()
dev.off() #clear graphics

plot(1)
plot(1, xlab = 'x-axis', ylab = 'y-axis', main = 'Main\nsubmain')
legend('topleft', col = c('red', 'blue'), pch = 19,
      legend = c('data1', 'data2'), title = 'Legend',
      cex = 2, pt.cex = 3, lwd = 2)

?par
par(tck = -0.01, mgp = c(1.3, 0.2, 0), mar = c(2.5, 2.5, 1.5, 0))
plot(1)

par(mfrow = c(2, 4))
layout.show(8)
par(mfrow = c(4, 4))
layout.show(16)
(m <- matrix(c(1,1,2,1,1,2,3,4,5), ncol = 3))
layout(m)
layout.show(5)
