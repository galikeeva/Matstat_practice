Stocs = read.csv('Task_3_var_6.csv')
head(Stocs)

(ncol <- dim(Stocs)[2])
(nrow <- dim(Stocs)[1])

por <- 0
K = 1000000
b <- K / (ncol * Stocs[1,])
for (i in 1:nrow){
  por[i] <- sum (K / (ncol * Stocs[1,]) * Stocs[i,])
}
Stocs <-cbind (Stocs, por)
rates <- Stocs[2:nrow,] / Stocs [1:(nrow-1),] - 1
head(rates)

del_s <- Stocs[1:(nrow - 1),1:ncol] * rates[,1:ncol]
head(del_s)
del_p <- 0
for (i in 1:(nrow-1)) {
  del_p[i] <- sum(b * del_s[i,])
}
head(del_p)

qqnorm(del_p)
hist(del_p)

vol_ds <- 0
mean_ds <- 0
Var <- cbind (del_s, del_p)
head(Var)
al = 0.95
Var_del <- Var[1,]
for(i in 1:(ncol + 1))
  mean_ds[i] <- mean(Var[,i])
  vol_ds[i] <- sd(Var[,i])
  Var_del[i] = qnorm(1-al,mean=mean(Var[,i]),sd=sd(Var[,i]))
Var_del

s_1 <- 0
mu <- 0
sig <- 0
Var_g <- 0
for (i in 1:10) {
  s_1[i] <- mean(Stocs[,i])
  mu[i] <- mean(rates[,i])
  sig[i] <- sd(rates[,i])
  Var_g[i] <- s_1[i] * (1 - exp(-mu[i] * nrow + qnorm (al, mean = mu[i], sd = sig[i]) * sig[i] * sqrt(nrow)))
}
Var_g

Var_h <-0
for (i in 1 : ncol){
  Var_h[i] <- (-1 * quantile(del_s[,i], cbind(1 - al), al))
}
Var_h
(-1 * quantile(del_p, cbind(1 - al), al))

for(i in 1:ncol){
  ES(del_s[,i], p=al, method="historical")
}
ES(del_p, p=al, method="historical")

(beta <- cov (rates[, 1:(ncol - 1)], rates[, ncol]) / var(rates[, ncol]))
r <- 0.07
(alpha_manual <- colMeans(rates[, 1:(ncol - 1)])- r- beta*(colMeans (rates[, 1:ncol - 1]) - r))

(mean (rates[, ncol + 1] - r) / sd (rates[, ncol + 1] - r))

sh_f <- 0 
del_v <- 0
for (i in 1:ncol) {
  d <- diff (Stocs[,i])
  m <- mean (d)
  sd <- sd (d)
  del_v[i] <- qnorm (1 - al, mean = m, sd = sd)
  sh_f[i] <- ((-1 * sd * exp (-(del_v[i] - m) * (del_v[i] - m) / (2 * sd * sd)))/ sqrt (2 * pi) + m * pnorm ((del_v[i] - m) / sd)) / (1 - al)
}
sh_f
