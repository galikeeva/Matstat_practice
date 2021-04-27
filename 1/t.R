Stocs <- read.csv("var_6.csv")
head(Stocs)

(ncol <- dim(Stocs)[2])
(nrow <- dim(Stocs)[1])
rates <- Stocs[2:nrow,] / Stocs [1:(nrow-1),] - 1
head(rates)
rates <- rates[1:(nrow-2),]

matplot(rates, type = 'l', main = "График доходностей", col = "blue", pch = 20)

pca <- prcomp(rates, scale = TRUE)
summary(pca)

pca$center
mp <- barplot(pca$sdev,main = "Standard deviation explained by the principal components.", col = "blue")

dim(pca$rotation)

ss <- summary(pca)
mp <- barplot(ss$importance[2,],main = "Importance of Factors.", col = "blue")


his <- ss$importance[3,]
his[7:10]<- NA
r <- as.matrix(cbind(ss$importance[3,],his))
mp <- matplot(1:10, r, type = 'h',lty = 1, lwd = 10,main = "Cumulative proportion.", col = c("blue",'red'))
abline(h = 0.8,col = 'black')

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
number <- 4
fscores <- scores(rates,pca,number)
matplot(fscores,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores')

c_1 <- cor(fscores[,1], rates$V1)
c_2 <- cor(fscores[,1], rates$V2)
c_3 <- cor(fscores[,1], rates$V3)
c_4 <- cor(fscores[,1], rates$V4)
c_5 <- cor(fscores[,1], rates$V5)
c_6 <- cor(fscores[,1], rates$V6)
c_7 <- cor(fscores[,1], rates$V7)
c_8 <- cor(fscores[,1], rates$V8)
c_9 <- cor(fscores[,1], rates$V9)
c_10 <- cor(fscores[,1], rates$V10)
cor <- c(c_1, c_2, c_3, c_4, c_5, c_6, c_7, c_8, c_9, c_10)
matplot(1:10, cor, type = 'h', lty = 1, lwd = 10, main = "Correlation")

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
rest <- restoreData(fscores,pca$rotation,pca$center)
head(rest[,2])

r <- unlist(rates[,2])
matplot(cbind(rest[,2],r),type ='b',pch=21,lwd = 2,main = 'Restored rates',col = c('blue','green'),lty = 1)
legend('topleft',c('original rates','restored'),lty=1,lwd=2,col=c('blue','green') )

