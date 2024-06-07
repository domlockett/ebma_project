setwd("~/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/")
library(foreign)
### Begin with a pure (or nearly so) replication of published 2008 predictions

# (Obviously, not a replication, but ...) ## NOTE: To make this comparable, we are going to shift to share of two-party vote
silver <- read.dta("silver2012prez.dta")
silver.in.08<-silver[silver$year<=2008,]

# Just out of curiosity, what was this model's out-of-sample prediction for 2008?
s.model <- lm(margin~approval+ideology+gdp, data=silver.in.08)
predict(s.model, newdata=silver[silver$year==2008,])


# Campbell Replication
campbell <- read.csv("Campbell_Data.csv")
campbell.in.08 <- campbell[campbell$ELECTION<=2004,]

c.model <- lm(INPTYVOTE~SEPTPOLL+GDPQTR2HALF, data=campbell.in.08)
summary(c.model)

# Lewis-Beck replication
lb <- read.csv("Lewis-Beck_Tien_Data.csv")
lb <- lb[1:16, 1:7]

lb.in.08 <- lb[lb$YEAR<=2004,]
lb.model <- lm(POP2PVOT ~ JULYPOP + INCXGNP + JOBHOUSU + CLOSEINC, data=lb.in.08)
summary(lb.model)

# Erickson-Wlezien Data

ew <- read.dta("Erikson_Wlezien_Data.dta")
ew$IncumbentPoll[13] <- ew$IncumbentPoll[14]

ew.in.08 <- ew[ew$Year<=2004 & ew$Cycle==14,]
ew.in.08 <- ew.in.08[!is.na(ew.in.08$Year),]
summary(lm(IncumbentVote ~ l1CumLEIGrowth + IncumbentPoll, data=ew.in.08))

#### Fair Data

fair <- read.csv("Fair_Data.csv")
fair <- fair[fair$Year>=1916 & fair$Year<=2004,]

fair$GI <- fair$G*fair$I
fair$PI <- fair$P*fair$I
fair$ZI <- fair$Z*fair$I
fair$WI <- fair$WAR*fair$I
fair$newI <- fair$I

fair.model <- lm(VP ~ GI + PI + ZI + DPER + DUR + newI + WAR , data=fair)
summary(fair.model)


### Hibbs model
hibbs <- read.dta("Hibbs.dta")

hibbs$r <- hibbs$dpi_pc/(hibbs$cpi_sa_8284/100)
hibbs$lnr <- log(hibbs$r)
hibbs$dnlr <- NA
for (i in 2:nrow(hibbs)){
  hibbs$dnlr[i] <- (hibbs$lnr[i]-hibbs$lnr[i-1])*400
}

for(i in 16:nrow(hibbs)){
  hibbs$dnlr.L1[i] <- hibbs$dnlr[i-1]
  hibbs$dnlr.L2[i] <- hibbs$dnlr[i-2]
  hibbs$dnlr.L3[i] <- hibbs$dnlr[i-3]
  hibbs$dnlr.L4[i] <- hibbs$dnlr[i-4]
  hibbs$dnlr.L5[i] <- hibbs$dnlr[i-5]
  hibbs$dnlr.L6[i] <- hibbs$dnlr[i-6]
  hibbs$dnlr.L7[i] <- hibbs$dnlr[i-7]
  hibbs$dnlr.L8[i] <- hibbs$dnlr[i-8]
  hibbs$dnlr.L9[i] <- hibbs$dnlr[i-9]
  hibbs$dnlr.L10[i] <- hibbs$dnlr[i-10]
  hibbs$dnlr.L11[i]<- hibbs$dnlr[i-11]
  hibbs$dnlr.L12[i] <- hibbs$dnlr[i-12]
  hibbs$dnlr.L13[i] <- hibbs$dnlr[i-13]
  hibbs$dnlr.L14[i] <- hibbs$dnlr[i-14]
}

hibbs.real <- hibbs[!is.na(hibbs$presvote),]

hibbs.model <- nls(presvote ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) + ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) + ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) + ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/(1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 + g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*Fatalities, start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=hibbs.real)

### Abrmowitz replications
ab <- read.csv("Abramowitz_data.csv")
ab <- ab[ab$year<=2004,]
ab.model <- lm(vote~q2gdp+term+juneapp,data=ab)
summary(ab.model)


####################################################################################
############################ Combining all data into a single dataset ##############
####################################################################################

   master.data <- data.frame(matrix(NA, 24, 1))
   colnames(master.data) <- "dv"
   rownames(master.data) <- seq(1916,2008, by=4)
   master.data$year <- seq(1916,2008, by=4)

# Add in  campbell data
   campbell <- read.csv("Campbell_Data.csv")
   master.data$dv[9:24] <- campbell$INPTYVOTE
   master.data$septpoll[9:24] <- campbell$SEPTPOLL
   master.data$gdpqtr2half[9:24] <- campbell$GDPQTR2HALF


# Add in Silver
   silver <- read.dta("silver2012prez.dta")
   master.data$approvals[8:24] <- silver$approval
   master.data$ideologys[8:24]<-silver$ideology
   master.data$gdps[8:24]<-silver$gdp


## Add in lewis-beck data
   lb <- read.csv("Lewis-Beck_Tien_Data.csv")
   lb <- lb[1:16, 1:7]
   master.data$julypop[9:24] <- lb$JULYPOP
   master.data$incxgnp[9:24] <- lb$INCXGNP
   master.data$jobhousu[9:24] <- lb$JOBHOUSU
   master.data$closeinc[9:24] <- lb$CLOSEINC


# Add in Erickson-Wlezien Data
   ew <- read.dta("Erikson_Wlezien_Data.dta")
   ew.red <- ew[ew$Year<=2008 & ew$Cycle==14,]
   ew.red <- ew.red[1:15,]
   master.data$l1cumleigrowth[10:24] <- ew.red$l1CumLEIGrowth
   master.data$incumbentpoll[10:24] <- ew.red$IncumbentPoll

#### Fair Data
    fair <- read.csv("Fair_Data.csv")
    fair <- fair[fair$Year>=1916 & fair$Year<=2008,]
    master.data$G <- fair$G
    master.data$P <- fair$P
    master.data$Z <- fair$Z
    master.data$adper <- abs(fair$DPER)
    master.data$adur <-  abs(fair$DUR)
    master.data$war <-  fair$WAR
    master.data$I <-  fair$I

# Note: using the data from Fair for the presidential vote before 1948, Campbell's data before taht
    temp.dv <- ((fair$VP*fair$I) + 50*(1-fair$I))
    master.data$dv[1:8] <- temp.dv[1:8]

### Add Hibbs data
hibbs <- read.dta("Hibbs.dta")
hibbs$r <- hibbs$dpi_pc/(hibbs$cpi_sa_8284/100)
hibbs$lnr <- log(hibbs$r)
hibbs$dnlr <- NA
for (i in 2:nrow(hibbs)){
  hibbs$dnlr[i] <- (hibbs$lnr[i]-hibbs$lnr[i-1])*400
}

for(i in 16:nrow(hibbs)){
  hibbs$dnlr.L1[i] <- hibbs$dnlr[i-1]
  hibbs$dnlr.L2[i] <- hibbs$dnlr[i-2]
  hibbs$dnlr.L3[i] <- hibbs$dnlr[i-3]
  hibbs$dnlr.L4[i] <- hibbs$dnlr[i-4]
  hibbs$dnlr.L5[i] <- hibbs$dnlr[i-5]
  hibbs$dnlr.L6[i] <- hibbs$dnlr[i-6]
  hibbs$dnlr.L7[i] <- hibbs$dnlr[i-7]
  hibbs$dnlr.L8[i] <- hibbs$dnlr[i-8]
  hibbs$dnlr.L9[i] <- hibbs$dnlr[i-9]
  hibbs$dnlr.L10[i] <- hibbs$dnlr[i-10]
  hibbs$dnlr.L11[i]<- hibbs$dnlr[i-11]
  hibbs$dnlr.L12[i] <- hibbs$dnlr[i-12]
  hibbs$dnlr.L13[i] <- hibbs$dnlr[i-13]
  hibbs$dnlr.L14[i] <- hibbs$dnlr[i-14]
}

hibbs.real<- hibbs[!is.na(hibbs$presvote),]

master.data$fatalities[10:24] <- hibbs.real$Fatalities
master.data$wtq16[10:24] <- hibbs.real$wtq16
master.data$dnlr[10:24]  <- hibbs.real$dnlr
master.data$dnlr.L1[10:24] <- hibbs.real$dnlr.L1
master.data$dnlr.L2[10:24] <- hibbs.real$dnlr.L2
master.data$dnlr.L3[10:24] <- hibbs.real$dnlr.L3
master.data$dnlr.L4[10:24] <- hibbs.real$dnlr.L4
master.data$dnlr.L5[10:24] <- hibbs.real$dnlr.L5
master.data$dnlr.L6[10:24] <- hibbs.real$dnlr.L6
master.data$dnlr.L7[10:24] <- hibbs.real$dnlr.L7
master.data$dnlr.L8[10:24] <- hibbs.real$dnlr.L8
master.data$dnlr.L9[10:24] <- hibbs.real$dnlr.L9
master.data$dnlr.L10[10:24] <- hibbs.real$dnlr.L10
master.data$dnlr.L11[10:24] <- hibbs.real$dnlr.L11
master.data$dnlr.L12[10:24] <- hibbs.real$dnlr.L12
master.data$dnlr.L13[10:24] <- hibbs.real$dnlr.L13
master.data$dnlr.L14[10:24] <- hibbs.real$dnlr.L14


### Add Abrmowitz 
ab <- read.csv("Abramowitz_data.csv")
master.data$q2gdp[9:24] <- ab$q2gdp
master.data$term[9:24] <- ab$term
master.data$juneapp[9:24] <- ab$juneapp


#############
## Make the 08 variables replication sets | writing to functionalize
############
# (1) The DV is incumbent party's share of 2-party vote
# (2) Training period is 1952-2000
# (3) Prediction is for 2008

library(ensembleBMA)
library(nls2)


# Take care of hibbs coverage intervals
# The error bars for the hibbs model were estimated using stata, and recorded in this CSV
hibbspreds <- read.csv("Prediction_hibbs.csv")

source("~/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/my.eBMAWithSilver.R")
# this is a function that runs all of the models for different in-sample out-of-sample periods
ty <-14 # The total number of years back in time used to "fit" the models
ta <- 1 # This is an optional parameter (that we don't use) that takes the forecasts to the power 1/a before averaging


fit.2008 <- my.eBMA(tyn=15, train.years=min(ty,14), a=ta)
fit.2004 <- my.eBMA(tyn=14, train.years=min(ty,13), a=ta)
fit.2000 <- my.eBMA(tyn=13, train.years=min(ty,12), a=ta)
fit.1996 <- my.eBMA(tyn=12, train.years=min(ty,11), a=ta)
fit.1992 <- my.eBMA(tyn=11, train.years=min(ty,10), a=ta)
fit.1988 <- my.eBMA(tyn=10, train.years=min(ty,9), a=ta)
fit.1984 <- my.eBMA(tyn=9, train.years=min(ty,8), a=ta)
fit.1980 <- my.eBMA(tyn=8, train.years=min(ty,7), a=ta)
fit.1976 <- my.eBMA(tyn=7, train.years=min(ty,6), a=ta)



## Pulling out the predicted errors for each model for each year
errors <- rbind(fit.1976$err, fit.1980$err, fit.1984$err, fit.1988$err, fit.1992$err, fit.1996$err ,fit.2000$err, fit.2004$err, fit.2008$err)
sqrt(colMeans(errors^2)) #RMSE
colMeans(abs(errors)) #Mean Absolute Error

## Coverage Rates
coverage.67 <- rbind(fit.1976$cov.67, fit.1980$cov.67, fit.1984$cov.67, fit.1988$cov.67, fit.1992$cov.67, fit.1996$cov.67 ,fit.2000$cov.67, fit.2004$cov.67, fit.2008$cov.67)  
colMeans(coverage.67)
coverage.90 <- rbind(fit.1976$cov.90, fit.1980$cov.90, fit.1984$cov.90, fit.1988$cov.90, fit.1992$cov.90, fit.1996$cov.90 ,fit.2000$cov.90, fit.2004$cov.90, fit.2008$cov.90)  
colMeans(coverage.90)

##### Fun summary plot #####
plot.fun <- function(obj=fit.2008, year="2008", left=35, right=65, right.tex=5){
  order.vec <- c(6,2,1,3,4,5, 0)
  ebma.vec <- 7
  plot(obj$out.data, order.vec, xlim=c(left, right), ylim=c(-1, 9), xlab="", ylab="", cex=1.25, pch=19, col=c(rep(1,6), "Red"))
  segments(obj$sixseven[,1], order.vec, obj$sixseven[,2], order.vec, lwd=3, col=c(rep(1,6), "Red"))
  segments(obj$ninezero[,1], order.vec, obj$ninezero[,2], order.vec, lwd=1, col=c(rep(1,6), "Red"))
  abline(v=obj$observed, lty=2)
#  mtext(text="x", side=1, line=0, at=obj$observed)
  points(obj$conf.int[4], ebma.vec,  pch=19, cex=1.25, col="DarkBlue")
  segments(obj$conf.int[3], ebma.vec, obj$conf.int[5], ebma.vec, lwd=3,col="DarkBlue")
  segments(obj$conf.int[2], ebma.vec, obj$conf.int[6], ebma.vec, lwd=1, col="DarkBlue")
  text(left+right.tex, 8.5 ,year, cex=2, col="gray")
  text(rep(left+right.tex, 6), order.vec+.35, colnames(obj$in.data), col=c(rep(1,6), "Red"))
  text(left+right.tex, ebma.vec+.35, "Ensemble", col="DarkBlue")
}
par(mfrow=c(3,3), mar=c(1.25,.5,.5,.5), las=1, tcl=.5, mgp=c(1.5,.25,0), yaxt="n")
plot.fun(obj=fit.2008, year="2008", left=30, right=57, right.tex=2.9)
plot.fun(obj=fit.2004, year="2004", left=44, right=63, right.tex=2.5)
plot.fun(obj=fit.2000, year="2000", left=42, right=60, right.tex=2.2)
plot.fun(obj=fit.1996, year="1996", left=47, right=62, right.tex=1.85)
plot.fun(obj=fit.1992, year="1992", right=60, right.tex=3.1)
plot.fun(obj=fit.1988, year="1988", right=61, left=45, right.tex=2)
plot.fun(obj=fit.1984, year="1984", left=47, right=66, right.tex=2.35)
plot.fun(obj=fit.1980, year="1980", left=10, right=85, right.tex=8.7)
plot.fun(obj=fit.1976, year="1976", left=24, right=77, right.tex=6.65)





