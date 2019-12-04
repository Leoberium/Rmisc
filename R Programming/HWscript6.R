# Лев Мазаев
library(RColorBrewer)
mat <- matrix(c(1, 1, 1, 1, 2, 3), ncol = 3)
layout(mat)
par(tcl = -0.2, mgp = c(2, 0.5, 0), mar = c(3.2, 3.6, 1.5, 0.75),
    cex.lab = 1.6, cex.axis = 1.4)

# Первый график
cyls <- sort(unique(mtcars$cyl))
cylpalette <- brewer.pal(8, 'Dark2')
plot(mtcars$disp, mtcars$mpg,
     ylab = "Miles/(US) gallon", xlab = "Displacement",
     type = "p", cex = 4, lwd = 0, pch = 19
     )
for (i in cyls) {
  points(mtcars$disp[mtcars$cyl == i],
         mtcars$mpg[mtcars$cyl == i], 
         col = cylpalette[i],
         cex = 3.2, lwd = 0, pch = 19)
}
legend('topright', col = cylpalette[cyls], 
       legend = cyls,
       cex = 1.5, title = "# of cylinders", x.intersp = 0,
       pt.cex = 3.2, pch = 19, lwd = 0)
# Второй график
z <- seq(0, 2*pi, length.out = 200)
plot(mtcars$disp, mtcars$mpg,
     ylab = "Miles/(US) gallon", xlab = "Displacement",
     type = "n",
     ylim = c(10, 36), xlim = c(50, 490))
for (i in cyls) {
  polygon(x = mean(mtcars$disp[mtcars$cyl == i]) + 2*sd(mtcars$disp[mtcars$cyl == i])*cos(z),
          y = mean(mtcars$mpg[mtcars$cyl == i]) + 2*sd(mtcars$mpg[mtcars$cyl == i])*sin(z),
          col = paste0(cylpalette[i], "AA"))
  text(x = mean(mtcars$disp[mtcars$cyl == i]), 
       y = mean(mtcars$mpg[mtcars$cyl == i]),
       labels = i, cex = 3)
}
text(x = 375, y = 30, labels = "diameter is 2 SD", cex = 1.5)

# Третий график
carbs <- sort(unique(mtcars$carb))
meanmpg <- tapply(mtcars$mpg, mtcars$carb, mean)
sdmpg <- tapply(mtcars$mpg, mtcars$carb, sd)
plot(carbs, meanmpg,
     ylab = "Miles/(US) gallon", xlab = "# Carburators",
     ylim = c(8, 38), type = "l", lwd = 2)
points(carbs, meanmpg, cex = 3.6, pch = 19, col = "white")
points(carbs, meanmpg, cex = 2, pch = 19)
segments(carbs, meanmpg - 2*sdmpg,
         carbs, meanmpg + 2*sdmpg,
         lwd = 2)
text(x = 6, y = 30, labels = "2SD CI", cex = 1.5)