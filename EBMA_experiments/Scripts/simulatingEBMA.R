library(gtools)
library(stringr)

set.seed(123)
N <- 400
nmod <- 4
W.matrix <- matrix(NA, nrow=N, ncol=nmod)
alpha <- c(10,5,3,1)
W.matrix <- rdirichlet(N, alpha) #drawing weights, given alphas specified above
# 
# 

select.vec1 <- rep(NA, N)
for(i in 1:N){
  select.vec1[i] <- which(rmultinom(n = 1, size = 1,prob = W.matrix[i,])==1)
}

# 
# ##creation of observations for predictions
m1 <- rnorm(N, 40, 10)
m2 <- rnorm(N, 0, 1)
m3 <- rnorm(N, -10, 10)
m4 <- rnorm(N, 15, 5)
predictions <- cbind(m1, m2, m3, m4)


# 
# ##creation of DV with use of selection vector
##creation of DV with use of selection vector
DV <- means <- matrix(NA, nrow=N)
for(i in 1:N){
   means[i,] <- predictions[i, select.vec1[i]]
}

DV<-rnorm(N, mean = means, sd=2)
# 
test.normal <- makeForecastData(.predCalibration=predictions[1:399,],.outcomeCalibration=DV[1:399]) #create the dataframe to run EBMA algorithm
thisEnsemble.em <- calibrateEnsemble(test.normal, model = "normal", useModelParams=F, const=0, method = "EM") #run EBMA algorithm on data
thisEnsemble.gibbs <- calibrateEnsemble(test.normal, model = "normal", useModelParams=F, modelPriors = rep(0.00001, 4), method = "gibbs") #run EBMA algorithm on data
weights<-apply(W.matrix,2,mean)

weights
thisEnsemble.em@modelWeights
colMeans(thisEnsemble.gibbs@posteriorWeights)

tyn=400
a=1
train.years=399
dates <- seq(as.Date("2010-01-01", ), length.out = 400, by = "1 day")
dates <- as.character(dates) %>% 
   str_remove("-") %>% 
   str_remove("-")

pred.date <- dates[400]
full.forecasts<-predictions
full.observed<-DV
library(ensembleBMA)
my.E.data <- ensembleData(forecasts=(full.forecasts)^(1/a), dates = dates, observations=full.observed,initializationTime=1, forecastHour=1) #Make a dataset of the appropriate format for the ensembleBMA package
fit.eBMA <- ensembleBMAnormal(my.E.data, trainingDays=train.years, dates=pred.date, minCRPS=FALSE,
                              control=controlBMAnormal(biasCorrection="none",tol=0.000000001))
round(weights,2)

round(thisEnsemble.em@modelWeights,2)
round(colMeans(thisEnsemble.gibbs@posteriorWeights),2)
round(fit.eBMA$weights,2)


#### with missingness
missing <- matrix(data <- sample(c(FALSE, TRUE), prob = c(0.9, 0.05),1600, replace = TRUE), nrow = 400)

missingPred <- predictions
missingPred[missing] <- NA

test.normal.missing <- makeForecastData(.predCalibration=missingPred,.outcomeCalibration=DV) #create the dataframe to run EBMA algorithm
thisEnsemble.em.missing <- calibrateEnsemble(test.normal.missing, model = "normal", useModelParams=T, const=0, method = "EM") #run EBMA algorithm on data
thisEnsemble.gibbs <- calibrateEnsemble(test.normal.missing, model = "normal", useModelParams=F, modelPriors = rep(0.00001, 4), const=0, method = "gibbs") #run EBMA algorithm on data

weights









.forecastData <- test.normal.missing# this.ForecastData#test.normal
maxIter=1e6
method="EM"
exp=numeric()
useModelParams = FALSE
predType="posteriorMedian"
const=0
W = rep(1/dim(.forecastData@predCalibration)[2],dim(.forecastData@predCalibration)[2])
iterations= 40000
burns = 20000
thinning = 20
modelPriors = rep(1/dim(.forecastData@predCalibration)[2],dim(.forecastData@predCalibration)[2])
# 
# #save simulated "true" weights
weights<-apply(W.matrix,2,mean)
# 
# #save difference in "true" and estimated weights
# error.em <- (thisEnsemble.em@modelWeights-weights)
# 
# error.gibbs <- (apply(thisEnsemble.gibbs@posteriorWeights,2,mean)-weights)
