####################################################################
#         Replication of Presidential Forecast Application         #
#            Montgomery, Hollenbach, Ward                          #
#         "Calibrating ensemle forecasting models                  #
#           sparse data in the social sciences"                    #
#		   International Journal of Forecasting                    #       
#                     2014                                         #
#                                                                  #
####################################################################




rm(list=ls())
library(EBMAforecast) ### version 0.43 for exact results
library(xtable)


### set working directory to appropriate directory
setwd("~/Dropbox/EBMA/IJF_Replication")

### load data 
pres.data <- read.csv("presidential_data.csv")


### create forecast data object
.FD <- makeForecastData(.predCalibration=pres.data[1:5,-c(1:2)]
                          ,.outcomeCalibration=pres.data[1:5,2]
                          ,.predTest=pres.data[6,-c(1,2)] 
                          ,.outcomeTest=pres.data[6,2] 
                          ,.modelNames=colnames(pres.data[,-c(1:2)])
                          )


### run Ensemble with c=0.05
ensemble <- calibrateEnsemble(.forecastData=.FD, model="normal", useModelParams=FALSE, const=0.05)

### Table 3
xtable(summary(ensemble, showCoefs=FALSE)@summaryData)

### EBMA prediction
ensemble@predTest




### Figure 3
pdf(width=6, height=6, file="presForecast.pdf")
par(mfrow=c(2,1), mar=c(2,2.5,2,.5), tcl=0, mgp=c(1.1,.1,0), cex.lab=.8, cex.main=.9)
plot(ensemble, subset=5, main="2008 (In-sample)", xLab="% Two party vote for incumbent")
plot(ensemble, subset=1, period="test", main="2012 (Out-of-sample)", xLab="% Two party vote for incumbent")
dev.off()

pdf(width=6, height=6, file="presForecast2012.pdf")
par(mfrow=c(2,1), mar=c(2,2.5,2,.5), tcl=0, mgp=c(1.1,.1,0), cex.lab=.8, cex.main=.9)
plot(ensemble, subset=5, main="2008 (In-sample)", xLab="% Two party vote for incumbent")
plot(ensemble, subset=1, period="test", main="2012 (Out-of-sample)", xLab="% Two party vote for incumbent")
dev.off()



### EBMA model with c=0.00
ensemble1 <- calibrateEnsemble(.forecastData=.FD, model="normal", useModelParams=FALSE, const=0.00)
### prediction with c=0
ensemble1@predTest


### EBMA model with c=0.1
ensemble2 <- calibrateEnsemble(.forecastData=.FD, model="normal", useModelParams=FALSE, const=0.1)
### prediction with c=0.1
ensemble2@predTest





### mean prediction
mean_pred2012 <- apply(pres.data[6,-c(1,2)] ,1, mean)
### median prediction
median_pred2012 <- apply(pres.data[6,-c(1,2)] ,1, median)

### Table 4
table4_prediction=c(mean_pred2012,median_pred2012,ensemble1@predTest[1],ensemble@predTest[1],ensemble2@predTest[1])
table4_prediction =round(table4_prediction,1)

### Calculate Error
table4_error=abs(table4_prediction-51.9)

table4 =as.data.frame(rbind(table4_prediction, table4_error))
names(table4)=c("Mean","Median","c=0","c=0.05","c=0.1")
rownames(table4)=c("2012 prediction","Absolute Error")
xtable(table4)






modelPreds <- ensemble@predTest[-1]

#95% Credible interval
touchyQuantBMANormal <- function (alpha, WEIGHTS, MEAN, SD, up, low) 
{
  z <- uniroot(ensembleBMA:::cdfBMAnormal, lower = low, upper = up, 
               WEIGHTS = WEIGHTS, MEAN = MEAN, SD = SD, offset = alpha)
  z$root
}


ensembleBMA:::quantBMAnormal(.05, ensemble@modelWeights, modelPreds, rep(sqrt(ensemble@variance), length(modelPreds)))
ensembleBMA:::quantBMAnormal(.95, ensemble@modelWeights, modelPreds, rep(sqrt(ensemble@variance), length(modelPreds)))


# Prob that Obama wins
1-ensembleBMA:::cdfBMAnormal(50, ensemble@modelWeights, modelPreds, rep(sqrt(ensemble@variance), length(modelPreds)), 0)

