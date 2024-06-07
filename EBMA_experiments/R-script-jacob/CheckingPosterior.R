
library(MCMCpack)
#set.seed(1234)

sigma.sq.samples<-matrix(NA, nrow=5000, ncol=3)

for(iter in 1:5000){
n.models<-10
n.obs<-500
sigma<-.5
x<-matrix(rnorm(n.models*n.obs), nrow=n.obs, ncol=n.models)
alpha<-(runif(n.models, 1, 10))
omega<-rdirichlet(1, alpha)
theta<-rmultinom(n.obs, size=1, prob=omega)
theta<-t(theta)
sum(theta[,1])/n.obs
omega[1]
tau<-apply(theta, 1, function(x) which(x==1))

## Now decide which x w want
mu<-rep(NA, n.obs)
for(i in 1:n.obs){
  mu[i]<-x[i, tau[i]]
}
## Now add noise
y<-rnorm(n.obs, mean=mu, sd=sigma)


#### Make R version of sampler

temp<-1/rgamma(1000, shape = (n.obs + 1)/2, rate=sum((y-mu)^2)/2)

sigma.sq.samples[iter,]<-quantile(temp, c(.025, .5, .975))


}



colMeans((sigma.sq.samples))




