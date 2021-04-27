train <- read.csv("train_var_6.csv",header=T)
test <- read.csv("test_var_6.csv",header=T)

library(randomForest)
tr <- as.data.frame(train)
tr_f <- train
tr_f$f = as.factor(tr_f$f)
set.seed(1)
rf_train <- randomForest(tr_f$f~.,ntree=335, data=tr_f,importance=TRUE)
predicted  <- predict(rf_train,newdata = test)
pred <- data.frame("Factor" = predicted)
write.csv(pred$Factor, "T7.csv", row.names = F)

library(neuralnet)
set.seed(1)
my.net <- neuralnet(tr_f$f~., data = train,err.fct="sse",rep=2)
testres <- compute(my.net,test,rep = 1)$net.result
names(testres) = c("A", "B", "C","D")
pred <- data.frame("Factor" = max.col(testres))
for(i in c(1:nrow(pred))){
  num = pred$Factor[i]
  num = as.integer(num)
  pred$Factor[i] = names(testres)[num]
}
write.csv(pred$Factor, "T7.csv", row.names = F)  


set.seed(20)
tr_n0 <- neuralnet(tr_f$f~., 
                   data = train, err.fct = "ce",
                   hidden =7, threshold = 0.01,
                   linear.output=FALSE, algorithm = "rprop+")
testres <- compute(tr_n0,test,rep = 1)$net.result
names(testres) = c("A", "B", "C","D")
pred <- data.frame("Factor" = max.col(testres))
for(i in c(1:nrow(pred))){
  num = pred$Factor[i]
  num = as.integer(num)
  pred$Factor[i] = names(testres)[num]
}
write.csv(pred$Factor, "T7.csv", row.names = F)                  
  

library(xgboost)                 
tm <- train
t<-tm$x1
letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("A") + 1L}
for(i in c(1:nrow(tm))){
  t[i] = as.integer(letter2num(tm$f[i]))
}
numClasses = max(t) + 1
param <- list("objective" = "multi:softmax" ,
              "eval_metric" = "mlogloss",
              "num_class" = numClasses)

tm = tm[,1:3]
set.seed(1)
cv_tr = xgb.cv(param = param,data = data.matrix(tm), label = t,
               nfold = 3, nrounds = 50,prediction=T,verbose=F)
cv_tr$evaluation_log$test_mlogloss_mean
(bestNR = which.min(cv_tr$evaluation_log$test_mlogloss_mean))
set.seed(1)
model_tr<-xgboost(param = param,data=data.matrix(tm), label = t,
                  nrounds=bestNR,verbose=F,save_period=NULL)
tm1 <- test
testXGBOOST <- predict(model_tr, data.matrix(tm1))
testXGBOOST
for(i in c(1:200)){
  num = testXGBOOST[i]
  num = as.integer(num)
  testXGBOOST[i] = names(testres)[num]
}
write.csv(testXGBOOST, "T7.csv", row.names = F)  

train <- data.frame(x1 = train$x1, x2 = train$x2, x3 = train$x3,
                    x4 = train$x1 + train$x2 - train$x3, f = train$f)
test <- data.frame(x1 = test$x1, x2 = test$x2, x3 = test$x3,
                    x4 = test$x1 + test$x2 - test$x3)
tr_f <- train
tr_f$f = as.factor(tr_f$f)
library(neuralnet)
set.seed(1)
my.net <- neuralnet(tr_f$f~., data = train,err.fct="sse",rep=2)
testres <- compute(my.net,test,rep = 1)$net.result
names(testres) = c("A", "B", "C","D")
pred <- data.frame("Factor" = max.col(testres))
for(i in c(1:nrow(pred))){
  num = pred$Factor[i]
  num = as.integer(num)
  pred$Factor[i] = names(testres)[num]
}
write.csv(pred$Factor, "T7.csv", row.names = F)  

set.seed(20)
tr_n0 <- neuralnet(tr_f$f~., 
                   data = train, err.fct = "ce",
                   hidden =2, threshold = 0.01,
                   linear.output=FALSE, algorithm = "rprop+")
testres <- compute(tr_n0,test,rep = 1)$net.result
names(testres) = c("A", "B", "C","D")
pred <- data.frame("Factor" = max.col(testres))
for(i in c(1:nrow(pred))){
  num = pred$Factor[i]
  num = as.integer(num)
  pred$Factor[i] = names(testres)[num]
}
write.csv(pred$Factor, "T7.csv", row.names = F)                  

