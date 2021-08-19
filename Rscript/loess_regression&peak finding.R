library(data.table)
library(ggplot2)

sp1=fread("Datafile/ex_spectrum1.csv")

x=sp1$`Raman shift`
y=sp1$Intensity
plot(x, y, col = 'Gray', type = 'p', cex=0.5)
lines(x, y, col = 'Gray')

y

is.vector(x)

y.smooth <- loess(y ~ x, span=0.03)$fitted

dd=data.frame(x, y,y.lm$fitted)
head(dd)


y.lm <- stats::loess(y ~ x, span=0.03)


y.smooth <- loess(y ~ x, span=0.03)$fitted
tt
tt <- loess.smooth(x,y, span = 0.03)
plot(tt$x, tt$y, col = 'Gray', type = 'p')
lines(tt$x, tt$y, col = 'Gray')

model <- lm(formula = sp1$Intensity ~ sp1$`Raman shift`, data = sp1)
t=loess.smooth(x,y,span = 0.03)
t$x


y2=model$fitted.values


plot(x, y.smooth, col = 'Gray', type = 'p')
lines(x, y.smooth, col = 'Gray')
lines(x, y2, col = 'red')



plot(tt$x, tt$y, col = 'Gray', type = 'p')
lines(tt$x, tt$y, col = 'Gray')


xp=seq(round(min(x),0),round(max(x),0),0.1)
y_pred=predict(y.lm,xp)

plot(x,y, type='p', cex=0.2)
lines(x, y, col = 'red')
lines(xp, y_pred, col = 'blue')


sp1
sp.smooth <- loess(sp1$Intensity ~ sp1$`Raman shift`, span=0.03)
sp.smooth$fitted

sp1
org=sp1

org <- sp1$Intensity
org_lm <- y.smooth
a=dist(rbind(org, org_lm))
a ##0.01

re_spec=data.table(xp, y_pred)
re_spec


library(zoo)
w=2
head(y.smooth)
head(y.max)
head(zoo(y.smooth))

y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
x.max <- rollapply(zoo(x), 2*w+1, median, align="center")
length(y.max)
length(x.max)

plot(x.max, y.max, col = 'Gray', type='l')
lines(x.max, y.max, col = 'SkyBlue', lwd = 2)

plot(x, y, col = 'Gray', type = 'l')
lines(x, y.smooth, col = 'Black')
lines(x.max, y.max, col = 'SkyBlue', lwd = 2)

legend('topleft', c('1', '2', '3'), cex=0.8, col=c('Gray', 'Black', 'SkyBlue'), lty=c(1,1,1));

n <- length(y)
delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
plot(x.max, delta, type='l')
abline(h = 0, lty='dotted', col = 'red')


argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(w, span) {
  peaks <- argmax(x, y, w=w, span=span)
  
  plot(x, y, cex=0.75, col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]), col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
}

test(2,0.03)
test(1,0.03)

w=1
span=0.03
peaks <- argmax(x, y, w=w, span=span)
peaks$y.hat
peak_point=as.data.table(cbind(x[peaks$i],peaks$y.hat[peaks$i]))
peak_point$V2=peak_point$V2+abs(min(peak_point$V2))
peak_point



library(broom)
library(tidyverse)
library(ggplot2)
theme_set(theme_bw())
# Create fake data

res=10
df=data.frame()
for (i in 1:nrow(peak_point)) {
  temp=as.data.frame(peak_point[i,])
  temp
  new=data.frame(x=xp, y=temp[,2]*dnorm(xp,mean=temp[,1],sd=res))
  new$factor=i
  
  df=rbind.data.frame(df,new)
}

df
head(df)

library(RColorBrewer)
#n <- 60
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))


ggplot(df, aes(x=x, y=y, group=factor, col=as.factor(factor))) + 
  geom_line()+
  scale_color_manual(values = col_vector)

head(df)
df_d=dcast(df, x~as.factor(factor), value.var = "y",mean)

df_d$total=rowSums(df_d[,-1])

ggplot(df_d, aes(x=x, y=total)) + 
  geom_line()




head(df_d)

t1=as.data.table(df_d[,c("x", "total")])
t1
re_spec
dim(t1)
dim(re_spec)

tt=cbind(t1, re_spec$y_pred)

b=dist(rbind(tt$total, tt$V2))
b ##0.25


sp1
sp2=fread("Datafile/ex_spectrum2.csv")


c=dist(rbind(sp1$Intensity,sp2$Intensity))

