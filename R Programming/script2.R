getwd()
setwd("R_training/")

TRUE
FALSE
T #в момент загрузки эти переменные те же TRUE и FALSE
F

NA #нет данных, пустая ячейка
1/0 #бесконечность
log(-1) #NaN - Not a Number
NULL #полное отсутствие данных, пустой объект
length(NA) #есть одно измерение
length(NULL) #нет измерений
x <- 1:10 
x[1] <- NA
x[1] <- NULL #fault

a <- 1
a <- "a"
a <- 'a'
.a <- "lol"
.a
new.var <- 1
new.var
a <- 1 #all variables are vectors
A <- 2
a <- c(T,T) #logical vector
typeof(a) #type of vector
a <- c(2L,1L) #integer vector
typeof(a)
typeof(a/2) #now double after division
d <- c(1,1.1,1e-10)
typeof(d)
s = c('a','b')
typeof(s) #character vector

c(1,1:3,c(2,1))
a <- 1:2
b <- 1.2:7
c <- c(a,b)
c
c(c+10)
typeof(c(T,F,1L,"t")) #char > double > integer > logical
T+T #sum of logical variables is integer
T+1.1
1 & F #1 - TRUE, 0 - FALSE
as.integer('10') #transform char to integer
as.logical(1) #transform to logical

f <- factor(c('f','m,','f'))
f #levels - which variables we can meet in the factor
f <- factor(c('f','f','f'),level <- c('f','m'))
f
typeof(f) #integer: levels are encoded with integers

length(f)
x <- 1:10
attr(x,'my.attr') <- 'new attr' #attribute added
attr(x,'names') <- paste0('name',1:10) #paste0 concatenates
names(x)[1] <- 'new.name' #access to names
x <- c(a=1,b=4,c=-1,d=-5,e=10)
names(x)
x
x[c(1,5,1)] #access by position
x[c('a','b')] #access by name
#if some name is used multiple times then only first inclusion will be showed
table(names(x))

x < 0
x[3:4]
x[x < 0] #access by logical operations
x[T]
x[c(T,F)] #C(T,F) repeated
which(x < 0)

x <- runif(100,0,5) #random uniform
sort(x)
y <- sin(x)
plot(x,y,t="l")
o <- order(x) #to see line
o[1:10]
plot(x[o],y[o],type='l')

10 * 2
10**2
10%%3
10%/%3 #integer division
10/3
10 > 3
10 >= 3
10 == 2
10 != 2
T & T
T & F
T | F #compares all
T || F
c(T,T) || c(F,T) #compares only first values

1:10 + 1:10
1:10 + 1:5 #repeats shorter
1:10 * 1:3
1:10 == 1:10

#
i <- 0
while(i<4){
  print(i)
  i <- i + 1
}
for(i in 4:-1){cat(i,' ')}

a <- 1
a[2] <- 10
a[10] <- 2
a
a <- character(10)
a <- numeric(10)
system.time(for(i in 1:3000000)a[i] <- i)
system.time(a<-numeric(3000000)) #better to create vector of needed size prior to filling of it, it's faster
options(digits = 8) #Сменить длину double типа
