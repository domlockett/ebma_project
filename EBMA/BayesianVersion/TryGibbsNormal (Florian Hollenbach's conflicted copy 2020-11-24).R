library(EBMAforecast)
data(presidentialForecast)

set.seed(123)
nob = 400
nmod = 4
W.matrix<-matrix(NA,nrow=nob,ncol=nmod)
al<-c(10,5,3,1)
W.matrix<-rdirichlet(nob, al) #drawing weights, given alphas specified above


##vector to select obs from different models	
prob<-rep(NA,nob)
prob<-runif(nob)

selection<-function(probability, matrix){
  models<-dim(matrix)[2]
  obs<-dim(matrix)[1]
  interval<-matrix
  for(i in 2:models){
    interval[,1]<-matrix[,1]
    interval[,i]<-rowSums(matrix[,1:i])
  }
  position<-matrix(as.numeric(((probability<=interval))),ncol=models,nrow=obs)
  pos<-rowSums(position)
  pos.1<-(pos*(-1))+(models+1)
  W.indicator<-pos.1
  return(W.indicator)
}

select.vec<-selection(prob,W.matrix)

##creation of observations for DV
Dependent<-matrix(ncol=nmod,nrow=nob,NA)
for(i in 1:nmod){
  Dependent[,i]<-rnorm(nob,runif(1,min=-10,max=10),runif(1,min=0,max=25))
}

##creation of DV with use of selection vector
DV<-means<-matrix(NA,nrow=nob)
for(i in 1:nob){
  means[i,]<-Dependent[i,select.vec[i]]
}

DV<-rnorm(nob,mean=means,sd=1)

test<-makeForecastData(.predCalibration=Dependent,.outcomeCalibration=DV) #create the dataframe to run EBMA algorithm
thisEnsemble<-calibrateEnsemble(test, model="normal", useModelParams=F, const=0) #run EBMA algorithm on data
#save simulated "true" weights
weights<-apply(W.matrix,2,mean)
#save difference in "true" and estimated weights
error<-(thisEnsemble@modelWeights-weights)


forecasts<-Dependent
outcome<-DV

w<-rep(1/num.models, num.models)
sigma<-1


x = GibbsNormal(c(outcome), as.matrix(forecasts),w, sigma, 1000,20,1)
colMeans(x$W_post)
