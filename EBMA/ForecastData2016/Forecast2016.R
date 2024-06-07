set.seed(12345)
library(Rcpp)
library(ggplot2)
library(EBMAforecast)

## running normal gibbs ebma on election data

Rcpp::sourceCpp('~/Dropbox/EBMA/BayesianVersion/normalGibbsMissing.cpp')

data = read.csv("~/Dropbox/EBMA/ForecastData2016/PresidentforUse.csv")
keep = which(names(data)%in%c("Abramowitz","Campbell", "EW","LB.T","Lockerbie","N.B","Fair", "Holbrook"))
dataPred = data[-7,keep]
dataOut = data[7,keep]
outcome = data$truth[-7]
dataOut

start.w = rep(1/dim(dataPred)[2], dim(dataPred)[2])
test_alpha = rep(0.4,dim(dataPred)[2])


X2 = GibbsNormalMissing(as.matrix(outcome),as.matrix(dataPred),start.w,test_alpha,1,2000000,500000,2)
  colMeans(X2$W)
Wmean = data.frame(matrix(colMeans(X2$W),ncol=dim(dataPred)[2],nrow = 1))
names(Wmean) = names(dataPred)
Wmedian = data.frame(matrix(apply(X2$W, 2, median),ncol=dim(dataPred)[2],nrow = 1))
names(Wmedian) = names(dataPred)
Wmedian

EBMApred = (X2$W%*%t(as.matrix(dataOut)))
head(EBMApred)
median(EBMApred)
mean(EBMApred)
quantile(EBMApred,c(0.025,0.975))



median(EBMApred) + (qnorm(0.975)*sqrt(median(X2$Sigma)))
median(EBMApred) - (qnorm(0.975)*sqrt(median(X2$Sigma)))

.sd = sqrt(median(X2$Sigma))
.means = as.matrix(dataOut)
.nModThis = dim(.means)[2]
cols=rep("gray50", 8)
.xMin <- min(.means)-2.5*.sd
.xMax <- max(.means)+2.5*.sd
.xAxis <- seq(.xMin, .xMax, by=.01)
.yAxis <- matrix(NA, .nModThis, length(.xAxis)) 
W <- Wmean
pdf("~/Dropbox/SFPresentation/Presentation/forJacob.pdf", height = 4, width=5.5)
par(mar=c(2,2,1,1), mgp=c(1,0,0), tcl=0)
for(i in 1:.nModThis){.yAxis[i,] <- dnorm(.xAxis, mean=.means[,i], sd=.sd)*W[1,i]}
.totals <- colSums(.yAxis)
plot(NULL, xlim=c(.xMin, .xMax), ylim=c(0,max(.totals)), main="Ensemble Prediction 2016", xlab="Predicted Voteshare Clinton", ylab="Posterior Density")
for(i in 1:.nModThis){lines(.xAxis, .yAxis[i,], type="l", lty=2, col=cols[i])}
lines(.xAxis, colSums(.yAxis), lwd=2)
rug(.means); rug(median(EBMApred), lwd=3)
abline(v=median(EBMApred), col="darkred", lwd=2, lty=3)
abline(v=50.8, col="darkblue", lwd=2, lty=3)
legend("topleft", lty=c(3,3), col=c("darkred", "darkblue"), c("Prediction", "Outcome"), cex=.8, bty="n")
dev.off()
#abline(v=.actual, lty=3)


#95% Credible interval
touchyQuantBMANormal <- function (alpha, WEIGHTS, MEAN, SD, up, low) 
{
  z <- uniroot(ensembleBMA:::cdfBMAnormal, lower = low, upper = up, 
               WEIGHTS = WEIGHTS, MEAN = MEAN, SD = SD, offset = alpha)
  z$root
}

ensembleBMA:::quantBMAnormal(.05, W, .means, rep(sqrt(median(X2$Sigma)), length(.means)))
ensembleBMA:::quantBMAnormal(.95, W, .means, rep(sqrt(median(X2$Sigma)), length(.means)))


quantile(colSums(.yAxis),c(0.025,0.975))











this.ForecastData<-makeForecastData(.predCalibration=dataPred,.outcomeCalibration=outcome, .predTest = dataOut, .modelNames=names(dataPred))
thisEnsemble<-calibrateEnsemble(this.ForecastData, model="normal", useModelParams=FALSE)
summary(thisEnsemble, Period="test", showCoefs=FALSE)
