library(xts)
rates <- read.csv("./rates_1.csv",header = TRUE )
head(rates)

(ndays  <-  dim(rates)[1])
(ncol  <-  dim(rates)[2])
tr_dates <- as.Date(rates[,1],"%m/%d/%Y")
rates.xts <- as.xts(rates[2:ncol], order.by = tr_dates)
head(rates.xts)
plot(rates.xts,type = 'b',pch =20,main = "US Treasury Rates")

pca <- prcomp(rates.xts, scale = FALSE)
summary(pca)

mp <- barplot(pca$sdev,main = "Standard deviation explained by the principal components.", col = "blue")

ss <- summary(pca)
mp <- barplot(ss$importance[2,],main = "Importance of Factors.", col = "blue")

his <- ss$importance[3,]
l <- length(his)
his[4:l]<- NA
r <- as.matrix(cbind(ss$importance[3,],his))
mp <- matplot(1:l,r,type = 'h',lty = 1, lwd = 10,main = "Cumulative proportion.", col = c("blue",'red')) # default
abline(h = 0.95,col = 'black')

scores <- function(ldata,pca,number)
{
  cdata <- ldata
  m <- dim(ldata)[2]
  for (i in 1:m)
    cdata[,i] <- ldata[,i] - pca$center[i]
  loads <- pca$rotation[,1:number]
  cdata <- as.matrix(cdata)
  f <- cdata %*% loads
  return (f)
}

number <- 3
fscores <- scores(rates.xts,ss,number)
shift <- fscores[,1]
twist <- fscores[,2]
butterfly <- fscores[,3]
shift
twist
butterfly

matplot(cbind(ss$rotation[,1],ss$rotation[,2],ss$rotation[,3]),type = 'b',pch=21,lwd = 2,
        col = c("blue","green","magenta"),main= "Shift,Twist,Butterfly",ylab = "loadings",xlab="maturity",lty=1 )
legend("bottomleft",c("Shift","Twist","Butterfly"),lty=c(1,1,1),lwd = 2,col = c("blue","green","magenta"))


matplot(fscores,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores')
legend('topleft',c('shift','twist','butterfly'),col = 1:number,lty = 1)
abline(h=0)

restoreData<- function(fscores,loadings,center)
{
  npca <- dim(fscores)[2]
  myeigen <- t(loadings[,1:npca])
  rest <- fscores %*%myeigen
  m <- length(center)
  if (m == dim(rest)[2])
  {
    for (i in 1:m)
      rest[,i] <- rest[,i]+center[i]
  }
  return(rest)
}

lastdate <- rates[ndays,1]
rest <- restoreData(fscores,ss$rotation,ss$center)
ndays <- dim(fscores)[1]
head(rest)
r <- unlist(rates.xts[ndays,])
r <- as.matrix(r,ncol = 1)
r <- t(r)
s <- as.matrix(rest[ndays,],ncol = 1)
title <- paste('Restored rates.',' Date =',lastdate)
matplot(cbind(s,r),type ='b',pch=21,lwd = 2,main = title,col = c('blue','green'),lty = 1)
legend('topleft',c('original rates','restored'),lty=1,lwd=2,col=c('blue','green'))

library(FinAna)
par <- 100
m <- 2
coupon <- 0.03
tau <- c(1/12,1/6, 0.25, 0.5,1,2,3,5,7,10,20,30)
N <- c(15,15,15,200,15,20,20,10,10,10,10,15)
last_true <- 1:12
last_res <- 1:12

for (i in 1:12){
  last_true[i] <- rates.xts[ndays,i]
  last_res[i] <- rest[ndays,i]
}
P_or <- 0
P_res <- 0
for (i in 1:12){
  P_or <-  P_or + N[i] * bond.price(par,coupon,tau[i], last_true[i],m)
  P_res <-  P_res + N[i] * bond.price(par,coupon,tau[i],last_res[i],m)
}
P_or
P_res

(twist[["2020-03-17"]] - twist[["2020-03-18"]])

