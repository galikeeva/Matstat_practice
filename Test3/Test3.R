dat <- read.csv("Test3_variant_6.csv")
acf(dat,lwd = 5, col = "blue")
pacf(dat,lwd = 5, col = "blue")
ml_ar <-  arima(dat,order = c(1,0,0),method="ML")
ml_ar$aic
ml_ar <-  arima(dat,order = c(3,0,0),method="ML")
ml_ar$aic

ml_ar$coef
res <- ml_ar$residuals
Box.test(res, lag = 6, type = "Ljung-Box", fitdf = 2)
