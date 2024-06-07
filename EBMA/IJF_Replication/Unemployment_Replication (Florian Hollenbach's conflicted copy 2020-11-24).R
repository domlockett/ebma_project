####################################################################
#         Replication of Unemployment Application                  #
#            Montgomery, Hollenbach, Ward                          #
#         "Calibrating ensemle forecasting models                  #
#           sparse data in the social sciences"                    #
#		   International Journal of Forecasting                    #       
#                     2014                                         #
#                                                                  #
####################################################################



rm(list=ls(all=TRUE))
library(multicore)
library(foreach)
library(doMC)
library(xtable)
library(EBMAforecast) ### version 0.43 for exact results

### set working directory to appropriate directory
setwd("~/Dropbox/EBMA/IJF_Replication/")


#read data of unemployment forecasts
ud <- read.csv("unemployment_data.csv", as.is=TRUE, header=TRUE, row.names=1)

### keep only forecasts four quarters into the future, starting in second quarter of 1972 and exclude oberservations with
ud4 <- subset(ud, variable=="UNEMP6")
ud4 <- subset(ud4, forecast.year.quarter>1971.2)
ud4 <- ud4[all(is.na(ud4[,-c(1:3)]))==FALSE,]
#ud4 <- ud4[rowMeans(is.na(ud4[,-c(1:3)]))!=1,]


### for forecast evaluation later
ud4Green <- ud4[,c("forecast.year.quarter", "greenbook", "variable")]
### rename greenbook prediction
colnames(ud4)[430] <- "XGB"



### create a function that runs EBMA algorithm over data with a specified window size, 
### selects only models with minimum number of forecasts in calibration set, 
### and specifies constants


Sweeper <- function(.windowSize=30, .minCal=15, .predYearQuarter="1980.1", .const=0, data=NULL, .imp=FALSE){
  .predThis <- which(data$forecast.year.quarter==.predYearQuarter)
  .theseRows <- (.predThis-.windowSize-1):(.predThis-1)
  .all <- length(.theseRows)
  .selector <- colSums(is.na(data[.theseRows,]))<=(.all-.minCal) & !is.na(data[.predThis,])
  .reduced <- data[.theseRows, .selector]

   # this code takes out any row where we now have no forecasts
  .rowSelector <- !(rowMeans(is.na(.reduced[,-(1:3)]))==1)
  .reduced <- .reduced[.rowSelector,]
  .target <- data[.predThis, .selector]

  ### selection as Forecastdata
  .FD <- makeForecastData(.predCalibration=.reduced[,-c(1:3)]
                          ,.outcomeCalibration=.reduced[,3]
                          ,.predTest=.target[,-c(1:3)]
                          ,.outcomeTest=.target[,3]
                          ,.modelNames=colnames(.reduced[,-c(1:3)])
                          )
  ### ensemble calibration code		
  ensemble <- calibrateEnsemble(.forecastData=.FD, model="normal", useModelParams=FALSE, const=.const, predType="posteriorMedian")
  output <- matrix(c(as.numeric(rownames(data[.predThis,])), ensemble@predTest[1], as.numeric(ensemble@modelWeights)), nrow=1)
  output <- data.frame(output)
  colnames(output) <- c("row", "EBMA", ensemble@modelNames)
  output
}



### set number of cores to your specific setting
registerDoMC(cores=4)

### run sweeper function on unemployment data for 4 different values of c (0,0.05,0.1,1)
thisSweep1<- ldply(as.character(ud4[41:146,1]), Sweeper, .windowSize=8, .minCal=4, .const=.00, .parallel=TRUE,  .imp=FALSE, data=ud4)
thisSweep2<- ldply(as.character(ud4[41:146,1]), Sweeper, .windowSize=8, .minCal=4, .const=.05, .parallel=TRUE,  .imp=FALSE, data=ud4)
thisSweep3<- ldply(as.character(ud4[41:146,1]), Sweeper, .windowSize=8, .minCal=4, .const=.10, .parallel=TRUE,  .imp=FALSE, data=ud4)
thisSweep4<- ldply(as.character(ud4[41:146,1]), Sweeper, .windowSize=8, .minCal=4, .const=1, .parallel=TRUE,  .imp=FALSE, data=ud4)


### functions to calculate predictive metrics (see Appendix)
modelFits <- function(.thisOutcome, .thisForecastMatrix, .thisBaseline){
	.absErr <- abs(.thisForecastMatrix-.thisOutcome)
  	.sqrErr <- .absErr^2
  	.ape <- .absErr/abs(.thisOutcome) # absolute percent error

 	### Mean absolute errror
  	mae <- colMeans(.absErr, na.rm=TRUE)

  	### Root mean squared error
  	rmse <-  sqrt(colMeans(.sqrErr, na.rm=TRUE))

  	### Median absolute deviation
  	mad <- apply(.absErr, 2, quantile, probs=.5, na.rm=TRUE)

  	### Root mean Squaqred logarithmic error
  	.rmsle <- function(x, y){
    	sqrt(mean((log(abs(x)+1)-log(abs(y)+1))^2, na.rm=TRUE))
  	}
  	rmsle <- apply(.thisForecastMatrix, 2, .rmsle, y=.thisOutcome)

  	### mean absolute percentage error
  	mape <- colMeans(.ape, na.rm=TRUE)*100

  	###median absolute percentage errro
  	meape <-  apply(.ape, 2, quantile, prob=.5, na.rm=TRUE)*100

  	### median relative absolute error
  	.eStar <- .thisOutcome-.thisBaseline
  	.e <- .thisOutcome-.thisForecastMatrix

  	mrae <- apply(abs(.e/.eStar), 2, quantile, probs=.5, na.rm=TRUE)

  	### percent worse
  	pw <- colMeans(abs(.e)>abs(.eStar), na.rm =TRUE)*100
  
  	out <- cbind(mae, rmse, mad, rmsle, mape, meape, mrae, pw)
  	return(out)
}




### take data, EBMA predictions from individual sweeps and evaluate
data=ud4
### four quarter predictions
.lagAmount=4

.lag <-c(rep(NA, .lagAmount), data[1:(nrow(data)-.lagAmount),3])
names(.lag) <- rownames(data)
### set model names -- the same across all sweeps
.modelNames <- colnames(thisSweep2)[-c(1:2)]

### predictions from the EBMA models
.ensemblePred1 <- thisSweep1$EBMA
.ensemblePred2 <- thisSweep2$EBMA
.ensemblePred3 <- thisSweep3$EBMA
.ensemblePred4 <- thisSweep4$EBMA

.theseRows <- as.character(thisSweep2$row)

### predictions of the individual models
.modelPreds <- data[.theseRows,.modelNames]

.modelWeights <- thisSweep2[,-c(1:2)]
### true data
.outcome <- data[.theseRows, 3]

.lag <- .lag[.theseRows]
.nrow=dim(.modelPreds)[1]; .ncol=dim(.modelPreds)[2]
modelOut <- modelFits(.outcome, .modelPreds, .lag)


### Table 5

### mean, median, & green book predictions
.mean <- rowMeans(data[.theseRows,-c(1:3)], na.rm=TRUE) # an alternate metric
.median <- apply(data[.theseRows,-c(1:3)], 1, quantile, probs=.5, na.rm=TRUE)
.green <- subset(ud4Green, forecast.year.quarter%in%data[.theseRows,1])$greenbook
.time <- subset(ud4Green, forecast.year.quarter%in%data[.theseRows,1])$forecast.year.quarter

### put together all forecasts
all <- cbind(.ensemblePred1, .ensemblePred2, .ensemblePred3, .ensemblePred4,.green, .mean, .median)
all <- all[!is.na(.green),]
.redOut <- .outcome[!is.na(.green)]
.redLag <- .lag[!is.na(.green)]
.outTable <- modelFits(.redOut, all, .redLag)
xtable(.outTable)


### create matrix with the columns being predictions from EBMA model with c= 0.05 
.ensemblePredMatrix <- matrix(.ensemblePred2, nrow=nrow(.modelPreds), ncol=ncol(.modelPreds))

### set NA when model prediction was NA
.ensemblePredMatrix[is.na(.modelPreds)] <- NA

###calculate all the fit statistics for each
ensembleOut <- modelFits(.outcome, .ensemblePredMatrix, .lag)


## Total number of forecasts each model made
count <- colSums(!is.na(.modelPreds))
length(count) # total number of models included

### For model comparison table, 
### calculate what pct of metrics EBMA is better than for each model 
pct_better<-rowMeans((modelOut-ensembleOut)>=0)*100
rownames<-seq(1,length(count))
### table with number of predictions each model made and how many metrics EBMA is better on compared to that model
for_table<-as.data.frame(cbind(count,pct_better))



### now create cells for Table 6
### cells by 
### EBBMA better on 0-25, 26-50 %, 51- 75% and 76-100 metrics
### for models with 0-10 pred, 11-30 pred, 30-60, greater than 60 predictions
 
cell_025_010<-sum(ifelse(for_table$count <11 & for_table$pct_better<26,1,0))
cell_2650_010<-sum(ifelse(for_table$count <11 & for_table$pct_better>25 &for_table$pct_better<51,1,0))
cell_5175_010<-sum(ifelse(for_table$count <11 & for_table$pct_better>50 &for_table$pct_better<76,1,0))
cell_76100_010<-sum(ifelse(for_table$count <11 & for_table$pct_better>75 &for_table$pct_better<101,1,0))
cell_025_1130<-sum(ifelse(for_table$count >10 &for_table$count<31 & for_table$pct_better<26,1,0))
cell_2650_1130<-sum(ifelse(for_table$count >10 &for_table$count<31 & for_table$pct_better>25 &for_table$pct_better<51,1,0))
cell_5175_1130<-sum(ifelse(for_table$count >10 & for_table$count<31 &for_table$pct_better>50 &for_table$pct_better<76,1,0))
cell_76100_1130<-sum(ifelse(for_table$count >10 &for_table$count<31 & for_table$pct_better>75 &for_table$pct_better<101,1,0))
cell_025_3160<-sum(ifelse(for_table$count >30 &for_table$count<61 & for_table$pct_better<26,1,0))
cell_2650_3160<-sum(ifelse(for_table$count >30 &for_table$count<61 & for_table$pct_better>25 &for_table$pct_better<51,1,0))
cell_5175_3160<-sum(ifelse(for_table$count >30 &for_table$count<61  & for_table$pct_better>50 &for_table$pct_better<76,1,0))
cell_76100_3160<-sum(ifelse(for_table$count >30 &for_table$count<61 & for_table$pct_better>75 &for_table$pct_better<101,1,0))
cell_025_61<-sum(ifelse(for_table$count >60 & for_table$pct_better<26,1,0))
cell_2650_61<-sum(ifelse(for_table$count >60 & for_table$pct_better>25 &for_table$pct_better<51,1,0))
cell_5175_61<-sum(ifelse(for_table$count >60 & for_table$pct_better>50 &for_table$pct_better<76,1,0))
cell_76100_61<-sum(ifelse(for_table$count >60 & for_table$pct_better>75 &for_table$pct_better<101,1,0))
mat<-matrix(ncol=4,nrow=4,c(cell_76100_010, cell_5175_010, cell_2650_010, cell_025_010, cell_76100_1130, cell_5175_1130, cell_2650_1130, cell_025_1130, cell_76100_3160, cell_5175_3160, cell_2650_3160, cell_025_3160, cell_76100_61, cell_5175_61, cell_2650_61, cell_025_61),byrow=F)

sum(mat)
colSums(mat)
matOut <- rbind(mat, colSums(mat))
### now calculate percentages
for(i in 1:4){
matOut[i,] <-  matOut[i,]/matOut[5,]
}


##### Table 6 in Paper
xtable(matOut)




### color for plot
mycols <- c("gray90",rgb(241,163,64,maxColorValue=255), rgb(230, 97, 1,maxColorValue=255), rgb(35, 132, 67,maxColorValue=255))


### Figure 4
pdf(height=4, width=7, file="mdwtimeSeries2.pdf")
par(mar=c(2,2,.5,2), mfrow=c(1,1), mgp=c(1,0.25,0), tcl=0)
mywidths <- c(10,8,4,4)
plot(.time, .redOut, type="l", lwd=mywidths[1], ylab="", xlab="", main="", bty="n", las=1,col=mycols[1],xlim=c(1981,2008),axes=F)
years<-c("1980","1985","1990","1995","2000","2005","2008")
axis(1,at=c(1980,1985,1990,1995,2000,2005,2008),labels=years,cex.axis=.9)
colnames(all)
for(i in 1:3){
  this <- c(2, 6, 7)
  lines(.time, all[,this[i]], col=mycols[i+1], lty=1, lwd=mywidths[i+1])
}
mytxtcol<-mycols
mytxtcol[1]<-"black"
legend(1998, 10, c("Observed", "EBMA", "Median", "Green Book"), col=mycols, lty=c(1),bty="n",lwd=5,text.col=mytxtcol)
dev.off()
