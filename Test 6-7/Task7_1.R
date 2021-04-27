Data <- read.csv("VAR6.csv",header=T)
head(Data)

ncol <- dim(Data)[2]
nrow <- dim(Data)[1]
rates <- Data[2:nrow,3:ncol] / Data [1:(nrow-1),3:ncol] - 1
head(rates)

d = sort(sample(nrow - 1, (nrow - 1)*.9))
train <- rates[d,]
train <- as.data.frame(train)
test<-rates[-d,]

train1<-data.frame(ROSNEFT=train$ROSNEFT/max(train$ROSNEFT),BRENT=train$BRENT/max(train$BRENT),
                   RTS=train$RTS/max(train$RTS),USD_RUB=train$USD_RUB/max(train$USD_RUB))
test1<-data.frame(BRENT=test$BRENT/max(test$BRENT),
                   RTS=test$RTS/max(test$RTS),USD_RUB=test$USD_RUB/max(test$USD_RUB))

library(neuralnet)
library(Metrics)
set.seed(20)
net0 <- neuralnet(ROSNEFT~., data = train1, err.fct="sse", rep=2) 
testres0 <- compute(net0,test1,rep = 2)
(rm0 <- rmse(test$ROSNEFT, testres0$net.result))
newdata <-cbind(testres0$net.result*max(test$ROSNEFT),test$ROSNEFT)
matplot(tail(newdata,50),type = "b",pch=21,col = c("blue","magenta"),lty = c(1,1),lwd = 3)

set.seed(20)
net1 <- neuralnet(train1$ROSNEFT~., data = train1, hidden = 1, err.fct="sse",rep=2) 
testres1 <- compute(net1,test1,rep = 1)
(rm1 <- rmse(test$ROSNEFT, testres1$net.result))
newdata <-cbind(testres1$net.result*max(test$ROSNEFT),test$ROSNEFT)
matplot(tail(newdata,50),type = "b",pch=21,col = c("blue","magenta"),lty = c(1,1),lwd = 3)

library(randomForest)
set.seed(1)
rf_train <- randomForest(train1$ROSNEFT~.,ntree=300, data=train1,importance=TRUE)
predicted  <- predict(rf_train,newdata = test1)
(rm_rf <- rmse(test$ROSNEFT, predicted))
cor(predicted,test$ROSNEFT)

newdata <-cbind(predicted*max(test$ROSNEFT),test$ROSNEFT)
matplot(tail(newdata,50),type = "b",pch=21,col = c("blue","magenta"),lty = c(1,1),lwd = 3)

my.lm <- lm(train1$ROSNEFT~., data = train1)
regr.res <- predict(my.lm,test1)
(rm_lm <- rmse(test$ROSNEFT, regr.res))
newdata <-cbind(regr.res*max(test$ROSNEFT),test$ROSNEFT)
matplot(tail(newdata,50),type = "b",pch=21,col = c("blue","magenta"),lty = c(1,1),lwd = 3)

real <- ((test$ROSNEFT+ 1)*Data [-d,3:ncol])$ROSNEFT
Real_R0 <- (testres0$net.result[1:30]*max(test$ROSNEFT) + 1)*Data [-d,3:ncol]
head(Real_R0)
(rm0 <- rmse(real, Real_R0$ROSNEFT))
(test$ROSNEFT+ 1)*Data [-d,3:4]
Data [-d,3]
test$ROSNEFT + 1

Real_R1 <- (testres1$net.result[1:30]*max(test$ROSNEFT) + 1)*Data [-d,3:ncol]
head(Real_R1)
(rm0 <- rmse(real, Real_R1$ROSNEFT))

Real_Rf <- (predicted[1:30]*max(test$ROSNEFT) + 1)*Data [-d,3:ncol]
head(Real_Rf)
(rm0 <- rmse(real, Real_Rf$ROSNEFT))

Real_lm <- (regr.res[1:30]*max(test$ROSNEFT) + 1)*Data [-d,3:ncol]
head(Real_lm)
(rm0 <- rmse(real, Real_lm$ROSNEFT))
