w <- c(.2, .5, .3)
s<- 2.5  
T=5000
K=3

X<-matrix(rnorm(T*K), ncol=K, nrow=T)

tau<-rep(NA, T)
for(i in 1:T){
  tau[i]<-  sample(c(1,2,3), 1, prob=w)
}
table(tau)/sum(table(tau))

y<-rep(NA, T)
for(i in 1:T){
  y[i]<-rnorm(1, mean=X[i,tau[i]], s)
}
y  

alpha <- c(1,1,1)


theta_function <- function(w,s,y,x) {
  
  theta<-matrix(NA, nrow=T, ncol=K)
  for(k in 1:K){
    theta[,k]<-w[k]*dnorm(y, mean=x[,k], sd=s)
  }
  theta<-theta/rowSums(theta)
  return(theta)
  
}

library(stats)
tau_function <- function(theta) {
  tau<-matrix(NA, ncol=1, nrow=nrow(theta))
  for(i in 1:nrow(theta)) {
    tau[i] <- seq(1:ncol(theta))[rmultinom(1, 1, theta[i,])==1]
  }
  return(tau)
}

eta_function <- function(alpha, taus) {
  n <- c(rep_len(NA, length.out = length(alpha)))
  
  for(a in 1:length(alpha)) {
    n[a] <- (alpha[a] + length(which(taus==a)))
  }
  return(n)
}

library(MCMCpack)

weights <- function(eta) {
  weights <- rdirichlet(n = 1, eta)
  return(weights)
}

sigma_square2 <- function(y,x,tau) {
  summed <- c(rep_len(NA, length.out = nrow(x)))
  for(i in 1:nrow(x)) {
    summed[i] <- x[i,tau[i]]
  }
  numer <- sum((y - summed)^2)
  degree <- (length(y) - 1)
  #  print(numer/degree)
  #  print(numer)
  #  print(degree)
  sigma <- 1/rgamma(1, shape=degree/2, scale=1/(numer/2))
  return(sigma)
}



#### Let's test it out
n.draws<-1000

w <- c(.2, .1, .4, .3)
s<- 1.5
T=5000
K=length(w)

X<-matrix(rnorm(T*K), ncol=K, nrow=T)

tau<-rep(NA, T)
for(i in 1:T){
  tau[i]<-  sample(1:K, 1, prob=w)
}
table(tau)/sum(table(tau))

y<-rep(NA, T)
for(i in 1:T){
  y[i]<-rnorm(1, mean=X[i,tau[i]], s)
}

test_X<-X
test_y<-y
test_alpha<-rep(1, K)

drawsW <- matrix(NA, ncol= length(w), nrow = n.draws)
drawsS<-rep(NA, n.draws)

start.w<-c(rep(1/length(w), rep(length(w))))
start.s<-1

for(i in 1:n.draws) {
  if(i ==1){
    new_theta <- theta_function(start.w,start.s,test_y,test_X)
  } else {
    new_theta <- theta_function(new_w,new_s,test_y,test_X)
  }
  new_tau <- tau_function(new_theta)
  new_eta <- eta_function(test_alpha, new_tau)
  new_w <- weights(new_eta)
  new_s <-sqrt(sigma_square2(y=test_y, x=test_X, tau=new_tau))
  drawsW[i,] <- new_w
  drawsS[i]<-new_s
}

colMeans(drawsW)
summary(drawsS)

test_alpha = c(1,1,1,1)
x = GibbsNormal(test_y,test_X,start.w,test_alpha,1,10000,2000,2)

dim(test_X)
missing = matrix(c(rbinom(5000,1,0.95),rep(1,10000),rbinom(5000,1,0.9)),nrow= 5000,ncol = 4)
test_X[missing == 0] = NA
x2 = GibbsNormalMissing(test_y,test_X,start.w,test_alpha,1,10000,2000,2)

colMeans(x[[1]])
colMeans(x2[[1]])

summary(x[[2]])
