library(EBMAforecast)
data(presidentialForecast)

forecasts<-presidentialForecast[,1:6]
outcome<-presidentialForecast[,"Actual"]



num.models<-ncol(forecasts)  
num.obs<-nrow(forecasts)  
#start values
w<-rep(1/num.models, num.models)
sigma<-1
num.iter<-10000
w.out<-matrix(NA, nrow=num.iter, ncol=num.models)
sigma.out<-rep(NA, num.iter)

for(t in 1:num.iter){

evalsEach = evalsEach2 <-matrix(NA, nrow=num.obs, ncol=num.models)
set.seed(12)
for(i in 1:num.obs){
  evalsEach[i,]<-dnorm(rep(outcome[i], num.models), (as.matrix(forecasts)[i,]), rep(sigma, num.models))
}


evalsAll<-evalsEach%*%w

theta<-matrix(NA, nrow=num.obs, ncol=num.models)
for(i in 1:num.obs){
   theta[i,]<-w*evalsEach[i,]/evalsAll[i]
}

tau<-rep(NA, num.obs)
for(i in 1:num.obs){
  tau[i]<-sample(1:num.models, size=1, replace=FALSE, prob=theta[i,])
}

eta<-rep(NA, num.models)
for(i in 1:num.models){
 eta[i]<-1+sum(tau==i)
}

library(MCMCpack)

temp<-rep(NA, num.obs)
for(i in 1:num.obs){
  temp[i]<-sum((theta[i,]*(outcome[i]-forecasts[i,])^2))
}
temp2<-sum(temp)/num.obs
temp2

w<-  rdirichlet(1, alpha=eta)[1,]
w.out[t,]<-w
sigma<-rinvgamma(1, shape=num.obs/2, scale=1/temp2)
sigma.out[t]<-sigma
}


colMeans(w.out)
plot(sigma.out, type="l")
plot(w.out[,1], type="l")

demo(presForecast)


