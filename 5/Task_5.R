data <- read.csv('var_6_Task_5.rds',header = T)
head(data)
library(TSA)
x1 <- data[,1]
matplot(x1,type='p',main='Series1',pch=21)
periodogram(x1,col = "blue",lwd = 2)
x2 <- data[,2]
matplot(x2,type='b',main='Series2',pch=21,col='purple')
dx2 <-diff(x2)
matplot(dx2,type='b',main='Diff. Series2',pch=21,col='purple')
periodogram(dx2,col = "blue",lwd = 2)
k=kernel('daniell',c(7,7,7))
sp3=spec(dx2,kernel=k,log='no',sub='',xlab='Frequency',
         ylab='Smoothed Sample Spectral Density',col = "blue",lwd = 2)

l <- Re(fft(log(abs(fft(dx2))), inverse = TRUE))
l[1]=0
barplot(l[1:100],col = "blue",main="Kepstr")
(kep_val <-l[8])

x3 <- data[,3]
matplot(x3,type='b',main='Series3',pch=20,col='blue')
y <-periodogram(x3,col = "blue",lwd = 2)
sd(y$spec)

ssp <- spec.pgram(x3,spans = c(7,7,7))
plot(ssp$freq,ssp$spec,type = 'b',pch = 20,main = "Series3. Smoothed Periodogram",col='blue')
sd(ssp$spec)
