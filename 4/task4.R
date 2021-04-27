library(copula)
data <- readRDS('var_6_Task_4.rds')
names(data)
plot(data$predictor,data$output,main = 'predictor vs output', 
     col = 'green',pch = 20,xlab = 'predictor', ylab = 'output')
ruble_brent <- cbind(data$predictor,data$output)
e_cop <-pobs(ruble_brent)
plot(e_cop[,1],e_cop[,2],pch = 21,main ="pobs(Ruble vs Brent) ",col = "blue")
#Normal copula
normal_copula<-normalCopula(param=0,dim=2)
#Student copula
t_copula <-ellipCopula(family = "t",param = 0,dim = 2)
#Frank copula
Frank_copula<-frankCopula(param=5,dim=2)
#Clayton
Clayton_copula<-claytonCopula(param=5,dim=2)
Gaussian.Copula.fit<-fitCopula(normal_copula, 
                               e_cop, 
                               method = "ml",
                               optim.method = "BFGS", 
                               optim.control = list(maxit=1000))
parameters <- Gaussian.Copula.fit@copula@parameters
parameters
t.Copula.fit<-fitCopula(t_copula, 
                        e_cop, 
                        method = "ml",
                        optim.method = "BFGS", 
                        optim.control = list(maxit=1000))
parameters <- t.Copula.fit@copula@parameters
parameters
Clayton.Copula.fit<-fitCopula(Clayton_copula, 
                              e_cop, 
                              method = "ml",
                              optim.method = "BFGS", 
                              optim.control = list(maxit=1000))
parameters <- Clayton.Copula.fit@copula@parameters
parameters
Frank.Copula.fit<-fitCopula(Frank_copula, 
                            e_cop, 
                            method = "ml",
                            optim.method = "BFGS", 
                            optim.control = list(maxit=1000))
parameters <- Frank.Copula.fit@copula@parameters
parameters
t.Copula.fit@loglik
Gaussian.Copula.fit@loglik
Frank.Copula.fit@loglik
Clayton.Copula.fit@loglik
best_parameters <- parameters
persp(t_copula, dCopula, main="pdf",xlab="u", ylab="v", zlab="c(u,v)")
contour(t_copula,dCopula, main="pdf",xlab="u", ylab="v")

data$predictor_DistrType
data$predictor_DistrParameters
data$output_DistrType
data$output_DistrParameters

predictor.copula <- pgamma(data$predictor,shape =  data$predictor_DistrParameters[1],rate=data$predictor_DistrParameters[2])
output.copula <- plnorm(data$output,mean = data$output_DistrParameters[1], sd = data$output_DistrParameters[2])
plot(predictor.copula,output.copula,main = 'predictor vs output. Marginal Distribution Copula',
     col = 'green',pch = 20,xlab = 'predictor', ylab = 'output')

quantileLevel <- function(numCopula,copula, theta,alpha)
{
  if (numCopula == 1)
  {
    #Gaussian    
    q <- pnorm(qnorm(alpha) *sqrt(1-theta*theta)  + theta* qnorm(copula[,1]))
  }
  if (numCopula == 2)
  {
    q <- pt(qt(alpha, df = theta[2] + 1) * sqrt((theta[2] + qt(copula[,1], df = theta[2])*qt(copula[,1], df = theta[2]))
            *(1 - theta[1]*theta[1]) / (theta[2] + 1)) + theta[1] * qt(copula[,1], df = theta[2]), df = theta[2])
    #Student
  }
  if (numCopula == 3)
  {
    #Frank
  }
  if (numCopula == 4)
  {
    #Clayton
  }
  return(q)  
}
copula <- cbind(predictor.copula,output.copula)
alpha <- 0.95
copulanum <- 2
parameters <- best_parameters
quantile <- quantileLevel(copulanum,copula, parameters,alpha)
(anomalindex <- which(copula[,2]>quantile))
plot(copula[,1],copula[,2],pch =20,col = "blue",main = "quatile level 95%")
points(copula[,1],quantile,col = "green",pch = 20)
points(copula[anomalindex,1],copula[anomalindex,2],col = "magenta",pch = 20)
anomal_predictor <- data$predictor[anomalindex]
anomal_output    <- data$output[anomalindex]
head(anomal_predictor)
head(anomal_output)

variant <- 6
copulaNames <- c("normal", "student", "clayton","frank")
copulaName <-copulaNames[copulanum]
copulaName
myResult <- list(variant = variant,
                 copulaName = copulaName,
                 predictor.copula = predictor.copula,
                 output.copula = output.copula,  
                 best_parameters = best_parameters,
                 anomal_predictor= anomal_predictor,
                 anomal_output= anomal_output)

saveRDS(myResult,"result.rds")
