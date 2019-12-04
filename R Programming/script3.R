x <- 1:10
x[2:3]
x[-(2:3)] #returns every element except notated with minus

x <- list(1:3,'a',c(T,F,T),c(2.3,1)) #list object
str(x) #structure of list
x <- list(list(1,2),3:4)
str(x)
y <- c(list(1,2),3:4) #concatenates all objects inside
str(y)

unlist(y) #back to vector
x <- list(int=1:3,char='a',var3=c(T,F,T),var4=c(2.3,1))
str(x)
names(x)
names(x)[1:2] <- c('var1','var2')
names(x)
x[1:2] #1 and 2 in list
x['var3'] #3 in list
x[c(T,F,T,F)] #1 and 3 in list
x[[1]] #1 element itself
x$var2
x$var4[2]

a <-  matrix(1:12,ncol=3)
a <- matrix(1,ncol=3,nrow=4)
length(a) #number of cells in the matrix
ncol(a)
nrow(a)
dim(a)
rownames(a) <- c('r1','r2','r3','r4')
colnames(a) <- c('c1','c2','c3')
dimnames(a)
attributes(a)
x <- 1:10
attr(x,'dim') = c(5,2)
x

a[1:2,]
a[1:2,c(3,1)]
a[c('r1','r4'),2:1]
a[a[,2]>6,]
a[1,]
a[1,,drop=FALSE] #1-row matrix

a > 4
a + 5
a / 1:4

which(a>5,arr.ind = T) 
a <- matrix(NA,ncol=5,nrow=5)
a <- rbind(a,1:5) #adding row
a <- cbind(a,NA) #adding column

x <- data.frame(a = 1:3,b=c('a','b','c')) #this structure supports cols of different types of elements
x

dim(x)
ncol(x)
nrow(x)
colnames(x)
rownames(x) <-  c('r1','r2','r3')
x <- data.frame(a = 1:3,b=c('a','b','c'),check.names = F)
#options(stringsAsFactors = F)
