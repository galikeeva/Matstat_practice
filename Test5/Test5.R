data <- read.csv("Dead.csv", header = TRUE)
data <- ts(data$Dead,start= c(5, 1), frequency = 31)
plot(data ,type = "l", col = "blue",lwd = 2,main = "dead")

library(tseries)
adf.test(data)
#будем считать, что стационарный

acf(data,lwd = 5, col = "blue")
pacf(data,lwd = 5, col = "blue")
#AR1s1n

library(TSA)
eacf(data)
#AR1MA2

model1 <- arima(data,order = c(1,0,0),seasonal = list(order= c(1,0,0),period = 31),method = "ML")
model1$aic
model2 <- arima(data,order = c(1,0,2),method = "ML")
model2$aic
#AR1MA2

model <- arima(data,order = c(1,0,2),method = "CSS")
res <- model$residuals
matplot(res,lwd=2,type="l",col="blue",main="Residuals")
qqnorm(res)
qqline(res)
Box.test(res, lag = 6, type = "Ljung-Box", fitdf = 3)
# остатки случайны

acf(res)
#не особо автокоррелированы, что-то модель не учитывает?? (должны быть меньше 0.08?)

plot(model,n.ahead=12,type='b',xlab='Time',col="blue",lty=3,lwd=1)
abline(h=coef(model)[names(coef(model))=='intercept'])
#прогноз и интревалы

past <- window(data,start = c(5,1),end = c(8,15))
future <- window(data,start = c(8,16))
model <- HoltWinters(past, gamma= FALSE)
pred <- predict(model,n.ahead =60)
plot(model,predicted.values = pred,lwd=2)
lines(future,col = "blue",lwd = 3)

