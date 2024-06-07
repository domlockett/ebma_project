## normal sim 

library(mvtnorm)
library(Rcpp)
library(doParallel)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gridExtra)

N_dist = c(10,50,500)
alpha_dist = seq(0.01,1,0.05)
iter = c(10000,100000,500000)
simulation_para = data.frame(do.call(rbind,replicate(100,as.matrix(expand.grid(N_dist, alpha_dist, iter)),simplify = F)))
names(simulation_para) = c("Ndist","alpha_dist","iter_dist")

Rcpp::sourceCpp('Dropbox/EBMA/BayesianVersion/normalGibbsMissing.cpp')

simu = function(N, alpha, iterations){
  N = N
  NoutSample = N*.1
  NinSample = N*.9
  
  K = nmod = 8
  w = c(0.3,0.1,0.05,0.03,0.22,0.12,0.08,0.1)

  mu = runif(8,-3,3)
  X <- rmvnorm(N,mu, sigma = diag(length(mu)))


  tau<-rep(NA, N)
  for(i in 1:N){
    tau[i]<-  sample(1:K, 1, prob=w)
  }
  #table(tau)/sum(table(tau))

  Y<-rep(NA, N)
  for(i in 1:N){
    Y[i]<-rnorm(1, mean=X[i,tau[i]], 2.5)
  }

  insample_X <- X[1:NinSample,]
  insample_Y <- Y[1:NinSample]
  test_alpha<-rep(alpha, K)

  outsample_X = X[(dim(X)[1]+1-NoutSample):dim(X)[1],]
  outsample_Y = Y[(length(Y)+1-NoutSample):length(Y)]
  
  start.w = rep(1/K,K)
  x = GibbsNormalMissing(insample_Y,as.matrix(insample_X),start.w,test_alpha,1,iterations,2000,2)
  
  if(NoutSample == 1){
    median_pred = apply(x$W%*%(as.matrix(outsample_X)),2,median)
    this.ForecastData<-makeForecastData(.predCalibration=as.matrix(insample_X),.outcomeCalibration=(insample_Y), .predTest = t(as.matrix(outsample_X)), .outcomeTest = (outsample_Y))
  }
  if(NoutSample > 1){
    median_pred = apply(x$W%*%t(as.matrix(outsample_X)),2,median)
    this.ForecastData<-makeForecastData(.predCalibration=as.matrix(insample_X),.outcomeCalibration=(insample_Y), .predTest = (as.matrix(outsample_X)), .outcomeTest = (outsample_Y))
  }
  absErrorGibbs = mean(abs(outsample_Y - median_pred))
  gibbs_w = colMeans(x$W)
  
  thisEnsemble<-calibrateEnsemble(this.ForecastData, model="normal", useModelParams=FALSE, exp = 3, predType = "posteriorMedian",tol = sqrt(.Machine$double.eps), const = 0)
  absErrorEM = mean(abs(outsample_Y - c(thisEnsemble@predTest[,1,1])))

  EM_w = thisEnsemble@modelWeights
  meanWeightErrorEM = mean(abs(w - EM_w))
  meanWeightErrorGibbs = mean(abs(w - gibbs_w))
  WeightError = c(meanWeightErrorEM,meanWeightErrorGibbs)
  PredError = c(absErrorEM,absErrorGibbs)
  model = c("EM","Gibbs")
  Nsim = c(N,N)
  alphaSim = c(alpha,alpha)
  iterSim = c(iterations,iterations)
  res = data.frame(WeightError,PredError,model,Nsim,alphaSim,iterSim)
  
  return(res)
}

cl <- makeCluster(4)
registerDoParallel(cl)

dim(simulation_para)
simulation_alpha = foreach(i = 1:dim(simulation_para)[1],
                                    .combine = bind_rows,
                                    .export = c("simulation_para","simu","GibbsNormalMissing"),
                                    .packages = c("EBMAforecast","Rcpp","mvtnorm")) %do% {
                                    res = simu(simulation_para$Ndist[i],simulation_para$alpha_dist[i],simulation_para$iter_dist[i])
                                    return(res)  
                                    }
setwd("~/Dropbox/EBMA/BayesianVersion/")
save(simulation_alpha, file= "simulation_ebma.rda")


simulation_alpha[2, ]
summary(simulation_alpha)
data = ddply(simulation_alpha, .(model, Nsim, alphaSim, iterSim), summarize, meanPredError = mean(PredError), meanWeightError = mean(WeightError))

dat1 = subset(data, Nsim == 500)
dat1$iterSim = as.factor(dat1$iterSim)
p1 = ggplot(data = dat1,aes(y = meanPredError, x = alphaSim, colour = model))
p1 = p1 + geom_point(aes(colour = model, shape = iterSim), size = 5) #+ geom_line(aes(colour = model, linetype = model, shape = model),size = 2.5)
#p1 = p1 + geom_rect(aes(xmax = iv + 0.05, xmin = iv - 0.05, ymin = rep(0, 8), ymax = diff), alpha= 0.3)
p1 = p1 + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
p1 = p1 + labs(title = "N = 50", y="Abs Mean Pred Error", x = "Alpha")+ theme(axis.text.x=element_text(size=13)) + theme(axis.text.y=element_text(size=15))+ theme(axis.title.x = element_text(size = 20),axis.title.y = element_text(size = 20))
p1 = p1 + scale_colour_manual(name = "Model", values = c("#e41a1c", "#984ea3")) + scale_shape_manual(name = "Model",  values = c(19,17, 15)) #+ scale_linetype_manual(name = "Model", values = c(1, 4))
p1 = p1 + theme(legend.position = c(.8, .8),legend.key = element_blank(), legend.background = element_rect(colour = "white"), legend.text = element_text(size = 15), legend.title = element_text(size = 15)) #+ scale_y_continuous(limits = c(0, 6.5))
plot(p1)
s
