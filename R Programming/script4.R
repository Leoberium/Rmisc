r = runif(1)
if (r > 0.5) {
  print('yes')
} else if (r > 0.8) {
    print('yyyyyes!')
} else {
  print('no')
}

for(i in 1:10) print(i)

i = 0
while (i < 4) {
  print(i)
  i = i + 1
}

i = 0
repeat{
  cat(i)
  i = i + 1
  if (i > 4) break
}

my.rbinom <- function(n, p = 0.5) {
  if (n < 0) return(NA)
  sum(runif(n) < p)
}
my.rbinom(0.1, n = 100)
plot(1:10,sin(1:10),t='l')

myFun <- function(x){
  a <- 100
  x <- 20
}

funs <-  list(mean,sd,var,median)
for(f in funs) print(f(1:10))

myApply <- function(d,fun) fun(d)
myApply(1:10, sd)

powerFactory = function(n){
  function(d) d ^ n 
}
p10 = powerFactory(10)
p10(2)

plotSin <- function(x,...) plot(x, sin(x),...)
plotSin(1:100/10, t = 'l', col = 'red', lwd = '3') 

1 + 1
"+"(1,1)
"["(10:20,3)

attributes(mtcars)
class(mtcars)

x <- 1:10
class(x) <- 'my' # new class for x
length.my <- function(x) runif(1) # special definition of length on the class "my"
length(x)

x <- 1:10
y <- x^2
m <- lm(y ~ x)
class(m)
plot(m) # because variable m has a class called lm
?plot.lm # specific type of plot function defined on the class lm 

text = c('hello', 'world')
substring(text, 1, 2) <- 'gg'
strsplit(text,'r|l')
paste(text[1],text[2],sep = '_')
paste(text, collapse = " ")
?paste
gsub('hello','goodbye',text)
grep('ll',text)
grepl('ll',text)
?grepl
install.packages('ape')
library('ape')
t <- nj(matrix(rnorm(100,1:10),ncol=10))
plot(t, type = 'fan')
class(t)
?plot.phylo
source("https://bioconductor.org/biocLite.R")
biocLite("edgeR")
