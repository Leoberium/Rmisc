#home work
library(seqinr)
randomText = function(a,l)
  paste0(sample(unique(unlist(strsplit(a,''))),l,replace = T),collapse = '')
table(strsplit(randomText('atgc',1e5),''))

ecoli = read.fasta('~/sequence.fasta',as.string = T)[[1]]
ec = substr(ecoli,1,1e5)


cw1 = function(t,l){
  i = 1:(nchar(t)-l+1)
  table(sapply(i,function(j)substr(t,j,j+l-1)))
}

cw2 = function(t,l){
  i = 1:(nchar(t)-l+1)
  table(substring(t,i,i+l-1))
}


cw3 = function(t,l){
  i = 1:(nchar(t)-l+1)
  t = strsplit(t,'')[[1]]
  table(sapply(i,function(j)paste(t[j:(j+l-1)],collapse = '')))
}

cw4 = function(t,l){
  i = 1:(nchar(t)-l+1)
  t = strsplit(t,'')[[1]]
  table(do.call(paste0,lapply(1:l,function(j)t[i+j-1])))
}

system.time({z1=cw1(ec,6)})
system.time({z2=cw2(ec,6)})
system.time({z3=cw3(ec,6)})
system.time({z4=count(strsplit(ec,'')[[1]],6)})
system.time({z5=cw4(ec,6)})
gtctagac
n = names(z1)
table(z1==z5[n])

hexamers = sort(cw4(ecoli,6))
hexamers = data.frame(obs = as.numeric(hexamers),row.names = names(hexamers))
head(hexamers,n = 20)
tail(hexamers)


nt = table(strsplit(ecoli,''))/nchar(ecoli)

hexamers$exp = sapply(strsplit(rownames(hexamers),''),function(s)prod(nt[s]))*sum(hexamers$obs)


plot(hexamers$exp,hexamers$obs,log='xy')
abline(a=0,b=1,col='red')

hist(hexamers$obs/hexamers$exp,200)
abline(v=0.1)
hexamers[hexamers$obs/hexamers$exp<0.1,]

tr = c(a='t',t='a',g='c',c='g')
hexamers$is.polyndrome = substr(rownames(hexamers),1,3) ==  sapply(strsplit(substr(rownames(hexamers),4,6),''),function(x)paste(tr[rev(x)],collapse = ''))
table(hexamers$is.polyndrome)
plot(hexamers$exp,hexamers$obs,log='xy',col=hexamers$is.polyndrome+1)
ratio= hexamers$obs/hexamers$exp
wilcox.test(ratio[hexamers$is.polyndrome],ratio[!hexamers$is.polyndrome],alternative = 'l')

## base graphics
x = seq(from=0,5,length.out=20)
y = 1-exp(-x)
y = rnorm(length(y),y,0.03)
plot(x,y)

par(mfrow=c(2,3))
plot(x,y,type='p',main='p')
plot(x,y,type='l',main='l')
plot(x,y,type='b',main='b')
plot(x,y,type='s',main='s')
plot(x,y,type='S',main='S')
plot(x,y,type='h',main='h')

plot(x,y,log='xy')

par(mfrow=c(1,1))
plot(1,t='n',xlim=c(0,1),ylim=c(0,6)) # создаем пустой график
for(i in 1:6)
  lines(0:1,c(i,i),lty=i,lwd=i) # добавляем линии


plot(rep(1:5,times=5),rep(5:1,each=5),pch=0:24,
     cex=seq(1,7,length.out=25),xlim=c(0,6),ylim=c(0,6))

# colors
colors()
col='#FF0000'
col='#FF000090'
rgb(red=1,green=1,blue=0)
#terrain.colors, heat.colors, rainbow, topo.colors
install.packages('RColorBrewer')
library(RColorBrewer)
ColorBrewer()
brewer.pal.info
display.brewer.pal(10,'Spectral')
display.brewer.pal(10,'RdBu')
brewer.pal(10,'RdBu')


par(mfrow=c(3,3))
plot(x,y,col=c('blue','red'),pch=19,main='by name')
plot(x,y,col=c('#FF4444','#0000FF40'),pch=19,main='by hex')
cols=rgb(runif(20),runif(20),runif(20))
plot(x,y,col=cols,pch=19,main='random')
cols = rgb(y^2,0,max(y^2)-y^2,maxColorValue=max(y^2))
plot(x,y,col=cols,pch=19,main='blue->red')
plot(x,y,col=terrain.colors(20),pch=19,main='terrain')
plot(x,y,col=heat.colors(20),pch=19,main='heat')
plot(x,y,col=topo.colors(20),pch=19,main='topo')
plot(x,y,col=rainbow(20),pch=19,main='rainbow')

par(mfrow=c(1,1))
plot(1,t='n',xlim=c(0,100),ylim=c(0,100))
lines(0:100,90+sin(0:100/10)*10)
x = 0:10*10
y = 90+cos(0:10)*10
points(x,y,pch=16)
segments(x,y+3,x,y-3)
text(x,y,labels=paste('x=',0:10*10,sep=''),adj=c(0,0))
polygon(c(0,10,20),c(0,20,0),col='blue')
polygon(30+10*sin(0:100/50*pi),30+10*cos(0:100/50*pi),
        col='red')
rect(40,40,60,60,density=20,col='green')
abline(h = 0:5*20,lty=2,col='gray') 
abline(v = 0:5*20,lty=3,col='black') # use grid() instead!
abline(a=0,b=1,lty=3,col='magenta')


x = -30:30/10
d = dnorm(x)
p = pnorm(x)
plot(x,d,t='l',col='red')
#lines(x,p,col='blue') # bad scale
lines(x,p*max(d),col='blue') #but now y-axix labels are wrong!
axis(4,at = 0:5*0.2*max(d),labels=0:5*0.2)
legend('topleft',col=c('red','blue'),lwd=1,legend=c('density','distribution'))

plot(1,t='n',
     main='first\nsecond row',
     xlab=expression(frac(sqrt(x+y),z^2)),
     ylab=expression(x %=~% y))
mtext('on the right',4,0)


par(tck=-0.02, 				# относительная длина отсечек на осях
    mgp=c(1.1,0.2,0),		# положение названия оси, подписей, и самой оси
    mar=c(2,3,1.0,0),		# размер полей графика
    oma=c(0,0,0,1),		# размер полей всего листа
    fg = 'white',			# цвет графика
    bg = 'black',			# цвет фона
    col.axis = 'red',		# цвет осей
    col.lab='yellow',
    col.main='green',
    font.main=3,			# шрифт
    las	= 1,				# ориентация подписей
    xpd = FALSE			# можно ли рисовать за 
    # пределами графика
)					# и многое другое
plot(1:10,1:10,t='s',main="in's in green")
dev.off()

par(tck=-0.01,mgp=c(1.3,0.2,0),mar=c(2.5,2.5,1.5,0),oma=c(0,0,0,1))

par(mfrow=c(2,3),oma=c(0,0,0,0),mar=c(0,0,0,0))
for(i in 1:6)
  plot(1,1,pch=as.character(i),xaxt='n',yaxt='n',cex=15)
layout.show(6)

par(mfcol=c(2,3),oma=c(0,0,0,0),mar=c(0,0,0,0))
layout.show(6)

layout(matrix(c(1,1,2,1,1,2,3,4,4),ncol=3))
layout.show(4)

# likely I'll stop here

par(fig=c(0,0.8,0,0.8),oma=c(0,0,0,0),mar=c(2,2,0,0))
x = rnorm(100)
y = rnorm(100,x)
plot(x,y,pch=19)
par(fig=c(0.05,0.4,0.5,0.8),new=TRUE)
plot(density(x),xaxt='n',yaxt='n',main='')
par(fig=c(0,0.8,0.8,1),new=TRUE)
boxplot(x,horizontal=TRUE)
par(fig=c(0.8,1,0,0.8),new=TRUE)
boxplot(y)




boxplot(lapply(1:4,function(x){rnorm(100,x)}),col=rainbow(4),
        range = 1.5,
        outline = TRUE,
        horizontal = FALSE,
        notch = TRUE,
        ylim=c(-2,10))
boxplot(lapply(1:4,function(x){rnorm(100,x+5)}),col=heat.colors(4),add=T)


x = c(rnorm(1000,0),rnorm(1000,3))
hist(x)
h = hist(x,breaks=100)
h = hist(x,breaks=-35:60/10) 
lines(density(x),col='blue')
d = density(x,from=-3,to=7,bw=0.3)
lines(d$x,d$y*sum(h$counts)*(h$breaks[2]-h$breaks[1]),col='red')


par(mfrow=c(1,3),tck=-0.02,mgp=c(1.1,0.2,0),mar=c(2,3,1.0,0),oma=c(0,0,0,1))
m = round(x,0)
f = rep(c('group1','group2'),each=1000)
tab=table(f,m)
tab


barplot(tab)
barplot(tab,beside=T)
barplot(t(tab),beside=T,col=rainbow(ncol(tab)))
pie(tab[1,])
pie(tab[2,])


x = rnorm(10000,0)
y = round(rnorm(10000,x)*2,0)
x = round(x*2,0)
tab=table(x,y)
tab
image(tab,col=heat.colors(100))
image(tab,col=rev(heat.colors(100)))
image(tab,col=topo.colors(100))


x = seq(from=0,5,length.out=100)
y = 1-exp(-x)
y = rnorm(length(y),y,0.1)
plot(x,y,pch=19)
lines(x,smooth(y),col='gray',lwd=3)
m = smooth.spline(x,y,df=2)
p = predict(m)
lines(p,col='red',lwd=3)
lines(predict(smooth.spline(x,y,df=3)),col='orange',lwd=3)
lines(predict(smooth.spline(x,y,df=7)),col='green',lwd=3)

x2 = x^2
m = lm(y ~ x + x2)
lines(x,predict(m),col='blue',lwd=3)
