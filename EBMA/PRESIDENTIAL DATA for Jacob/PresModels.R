setwd("/Volumes/ICEWS Project/PRESIDENTIAL DATA for Jacob")
library(foreign)

### Begin with a pure (or nearly so) replication of published 2008 predictions

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

### Make a master dataset with everything needed from 1952-2008

   master.data <- data.frame(matrix(NA, 24, 1))
   colnames(master.data) <- "dv"
   rownames(master.data) <- seq(1916,2008, by=4)
   master.data$year <- seq(1916,2008, by=4)

# Add in  campbell data
   campbell <- read.csv("Campbell_Data.csv")
   #campbell <- campbell[campbell$ELECTION>=1952,]
   master.data$dv[9:24] <- campbell$INPTYVOTE
   master.data$septpoll[9:24] <- campbell$SEPTPOLL
   master.data$gdpqtr2half[9:24] <- campbell$GDPQTR2HALF


## Add in lewis-beck data

   lb <- read.csv("Lewis-Beck_Tien_Data.csv")
   lb <- lb[1:16, 1:7]

   master.data$julypop[9:24] <- lb$JULYPOP
   master.data$incxgnp[9:24] <- lb$INCXGNP
   master.data$jobhousu[9:24] <- lb$JOBHOUSU
   master.data$closeinc[9:24] <- lb$CLOSEINC


# Add in Erickson-Wlezien Data

   ew <- read.dta("Erikson_Wlezien_Data.dta")
   ew$IncumbentPoll[13] <- ew$IncumbentPoll[14]

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

#hibbs.model <- nls(dv ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) + ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) + ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) + ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/(1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 + g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*fatalities, start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=master.data)


### Add Abrmowitz 
ab <- read.csv("Abramowitz_data.csv")

master.data$q2gdp[9:24] <- ab$q2gdp
master.data$term[9:24] <- ab$term
master.data$juneapp[9:24] <- ab$juneapp



#ab.model <- lm(vote~q2gdp+term+juneapp,data=ab)
#summary(ab.model)


#############
## Make the 08 variables replication sets | writing to functionalize
############
# (1) The DV is incumbent party's share of 2-party vote
# (2) Training period is 1952-2000
# (3) Prediction is for 2008str


# Take care of hibbs coverage intervals

hibbspreds <- read.csv("Prediction_hibbs.csv")


source("/Users/jacobmontgomery/RFiles/Ward/CourtsModelFun.R")
ty <-14
ta <- 1
source("/Users/jacobmontgomery/RFiles/Ward/new.Plotter.R")
par(mfrow=c(2,1), mar=c(2.25,2.5,2,1), las=1, tcl=.5, mgp=c(1.5,.25,0))
fit.2008 <- my.eBMA(tyn=15, train.years=min(ty,14), a=ta)
title("2008", line=.25)
fit.2004 <- my.eBMA(tyn=14, train.years=min(ty,13), a=ta)
title("2004", line=.25)
fit.2000 <- my.eBMA(tyn=13, train.years=min(ty,12), a=ta)
fit.1996 <- my.eBMA(tyn=12, train.years=min(ty,11), a=ta)
fit.1992 <- my.eBMA(tyn=11, train.years=min(ty,10), a=ta)
fit.1988 <- my.eBMA(tyn=10, train.years=min(ty,9), a=ta)
fit.1984 <- my.eBMA(tyn=9, train.years=min(ty,8), a=ta)
fit.1980 <- my.eBMA(tyn=8, train.years=min(ty,7), a=ta)
fit.1976 <- my.eBMA(tyn=7, train.years=min(ty,6), a=ta)
#fit.1972 <- my.eBMA(tyn=6, train.years=4, a=ta)
#fit.1968 <- my.eBMA(tyn=5, train.years=4, a=ta)
errors <- rbind(fit.1976$err, fit.1980$err, fit.1984$err, fit.1988$err, fit.1992$err, fit.1996$err ,fit.2000$err, fit.2004$err, fit.2008$err)
sqrt(colMeans(errors^2))
colMeans(abs(errors))


#errors
coverage.67 <- rbind(fit.1976$cov.67, fit.1980$cov.67, fit.1984$cov.67, fit.1988$cov.67, fit.1992$cov.67, fit.1996$cov.67 ,fit.2000$cov.67, fit.2004$cov.67, fit.2008$cov.67)  
colMeans(coverage.67)
#coverage.95 <- rbind(fit.1976$cov.95, fit.1980$cov.95, fit.1984$cov.95, fit.1988$cov.95, fit.1992$cov.95, fit.1996$cov.95 ,fit.2000$cov.95, fit.2004$cov.95, fit.2008$cov.95)  
#colMeans(coverage.95)
coverage.90 <- rbind(fit.1976$cov.90, fit.1980$cov.90, fit.1984$cov.90, fit.1988$cov.90, fit.1992$cov.90, fit.1996$cov.90 ,fit.2000$cov.90, fit.2004$cov.90, fit.2008$cov.90)  
colMeans(coverage.90)

fit.

##### New magic plot #####
plot.fun <- function(obj=fit.2008, year="2008", left=35, right=65, right.tex=5){
  order.vec <- c(6+2,2,1,3+1,4+2,5+2)
  ebma.vec <- 7+3
  plot(obj$out.data, order.vec, xlim=c(left, right), ylim=c(0, 12.5), xlab="", ylab="", cex=1.25, pch=19)
  segments(obj$sixseven[,1], order.vec, obj$sixseven[,2], order.vec, lwd=3)
  segments(obj$ninezero[,1], order.vec, obj$ninezero[,2], order.vec, lwd=1)
  abline(v=obj$observed, lty=2)
  points(obj$conf.int[4], ebma.vec,  pch=19, cex=1.25)
  segments(obj$conf.int[3], ebma.vec, obj$conf.int[5], ebma.vec, lwd=3)
  segments(obj$conf.int[2], ebma.vec, obj$conf.int[6], ebma.vec, lwd=1)
  text(left+right.tex, 12 ,year, cex=2, col="gray")
  text(rep(left+right.tex, 6), order.vec+.35, colnames(obj$in.data))
  text(left+right.tex, ebma.vec+.35, "EBMA")
}
par(mfrow=c(3,3), mar=c(1.25,.5,.5,.5), las=1, tcl=.5, mgp=c(1.5,.25,0), yaxt="n")
plot.fun(obj=fit.2008, year="2008", left=35, right=57, right.tex=2.9)
plot.fun(obj=fit.2004, year="2004", left=44, right=63, right.tex=2.5)
plot.fun(obj=fit.2000, year="2000", left=42, right=60, right.tex=2.2)
plot.fun(obj=fit.1996, year="1996", left=47, right=62, right.tex=1.85)
plot.fun(obj=fit.1992, year="1992", right=60, right.tex=3.1)
plot.fun(obj=fit.1988, year="1988", right=61, left=45, right.tex=2)
plot.fun(obj=fit.1984, year="1984", left=47, right=66, right.tex=2.35)
plot.fun(obj=fit.1980, year="1980", left=10, right=85, right.tex=8.7)
plot.fun(obj=fit.1976, year="1976", left=24, right=77, right.tex=6.65)


#### year by Model
fit.list <- list(fit.2008, fit.2004, fit.2000, fit.1996, fit.1992, fit.1988, fit.1984, fit.1980, fit.1976)
par(yaxt="n", las=1)
plot(NULL, xlim=c(42, 62), ylim=c(1,9), main="EBMA", xlab="Percentage of two party vote for incumbent party", ylab="")
for (i in 1:9){
   points(fit.list[[i]][["conf.int"]][4], i, pch=19)
   segments(fit.list[[i]][["conf.int"]][2], i, fit.list[[i]][["conf.int"]][6], i)
   points(fit.list[[i]]$observed, i, pch="X")
   mtext(hibbspreds$Year[10-i], side=2, line=1, at=i)
}




detach("package:ensembleBMA")
library(verification)




sds <- rep(NA, 9)
preds <- matrix(NA,  9, 6)
observed <- rep(NA, 9)
ebma.crps <- rep(NA,9)
for (i in 1:9){
  sds[i] <- (fit.list[[i]]$fit.eBMA$sd)
  preds[i,] <- fit.list[[i]]$out.data
  observed[i] <- fit.list[[i]]$observed
  ebma.crps[i] <- fit.list[[i]]$this.crps[2]
}

my.CRPS <- rep(NA, 6)
for (i in 1:6){
  this.dat <- data.frame(preds[,i], sds)
  my.CRPS[i] <- crps(observed, this.dat)$CRPS
}
my.CRPS <- c(my.CRPS,  mean(ebma.crps))
names(my.CRPS) <- colnames(errors)
my.CRPS


### For first table
EBMA.errors <- ((fit.2008$in.data)%*%fit.2008$fit.eBMA$weights)-(fit.2008$full.observed[1:14])
model.errors.2008 <- fit.2008$in.data-fit.2008$full.observed[1:14]
colMeans(abs(model.errors.2008))
sqrt(colMeans((model.errors.2008^2)))


EBMA.errors <- ((fit.2004$in.data)%*%fit.2004$fit.eBMA$weights)-(fit.2004$full.observed[1:13])
model.errors.2004 <- fit.2004$in.data-fit.2004$full.observed[1:13]
sqrt(colMeans((model.errors.2004^2)))
colMeans(abs(model.errors.2004))






######## THE END


####

 # Stupid thing
   dates <- rep(NA, 10)
   for (i in 1:11){
     dates[i] <- paste("2011", "01", 10+i, "01", sep="")
    }

try.in <- rbind(fit.1968$out.data, fit.1972$out.data, fit.1976$out.data, fit.1980$out.data, fit.1984$out.data, fit.1988$out.data, fit.1992$out.data, fit.1996$out.data ,fit.2000$out.data, fit.2004$out.data, fit.2008$out.data)  

try.E.data <- ensembleData(forecasts=try.in, dates=dates,  observations=master.data$dv[14:24],
                             initializationTime=1, forecastHour=1)
i <- 9
try.fit <- ensembleBMAnormal(try.E.data, trainingDays=8, dates=dates[i],
                              control=controlBMAnormal(biasCorrection="additive"), minCRPS=T)
plot(try.fit, try.E.data, ask=F)


### Now use data to make a prediction where:
# (1) The DV is incumbent party's share of 2-party vote
# (2) Training period is 1952-2000
# (3) Prediction is for 2004

in.data.04 <- matrix(NA,  13, 6)
out.data.04 <- matrix(NA, 1, 6)
rownames(in.data.04) <- seq(1952,2001, by=4)
rownames(out.data.04) <- 2004

# Campbell
campbell.in.04 <- campbell.in.08[campbell.in.08$year<=2000,]
campbell.out.04 <- campbell.in.08[campbell.in.08$year==2004,]

c.model <- lm(INPTYVOTE~SEPTPOLL+GDPQTR2HALF, data=campbell.in.04)
summary(c.model)

in.data.04[,1] <- fitted(c.model)
out.data.04[,1] <- predict(c.model, newdata=campbell.out.04)

## Lewis-Beck
lb.in.04 <- lb.in.08[lb.in.08$year<=2000,]
lb.out.04 <- lb.in.08[lb.in.08$year==2004,]

lb.model <- lm(dv ~ JULYPOP + INCXGNP + JOBHOUSU + CLOSEINC, data=lb.in.04)
summary(lb.model)
in.data.04[,2] <- fitted(lb.model)
out.data.04[,2] <- predict(lb.model, newdata=lb.out.04)

colnames(out.data.04) <- colnames(in.data.04) <- c("Campbell", "Lewis-Beck", "Erickson", "Fair", "Hibbs", "Abramowitz")

# Erickson-Wlezien Data

ew.in.04 <- ew.in.08[ew.in.08$year<=2000,]
ew.out.04 <- ew.in.08[ew.in.08$year==2004,]

ew.model <- lm(IncumbentVote ~ l1CumLEIGrowth + IncumbentPoll, data=ew.in.04)
summary(ew.model)

in.data.04[,3] <- fitted(ew.model)
out.data.04[,3] <- predict(ew.model, newdata=ew.out.04)

#### Fair Data
fair.in.04 <- fair.in.08[fair.in.08$year<=2000,]
fair.out.04 <- fair.in.08[fair.in.08$year==2004,]


final.fair.model <- lm(dv ~ G + P + Z + aDPER + aDUR + WAR + newI, data=fair.in.04)
summary(final.fair.model)
in.data.04[,4] <- fitted(final.fair.model)
out.data.04[,4] <- predict(final.fair.model, newdata=fair.out.04)

### Hibbs model
hibbs.in.04 <- hibbs.in.08[hibbs.in.08$year<=2000,]
hibbs.out.04 <- hibbs.in.08[hibbs.in.08$year==2004,]

hibbs.model <- nls(dv ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) + ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) + ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) + ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/(1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 + g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*Fatalities, start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=hibbs.in.04)
summary(hibbs.model)
in.data.04[,5] <- fitted(hibbs.model)
out.data.04[,5] <- predict(hibbs.model, newdata=hibbs.out.04)

### Abrmowitz replications
ab.in.04 <- ab.in.08[ab.in.08$year<=2000,]
ab.out.04 <- ab.in.08[ab.in.08$year==2004,]

ab.model <- lm(dv~q2gdp+term+juneapp,data=ab.in.04)
summary(ab.model)
in.data.04[,6] <- fitted(ab.model)
out.data.04[,6] <- predict(ab.model, newdata=ab.out.04)

##### use ebma package

full.forecasts.04 <- rbind(in.data.04, out.data.04)
full.observed.04 <- c(campbell$INPTYVOTE[2:15])

dates <- dates[1:14]

my.E.data.04 <- ensembleData(forecasts=(full.forecasts.04^(1/a)), dates=dates, observations=full.observed.04, initializationTime=1, forecastHour=1)

# 2004 fitting
fit.2004 <- ensembleBMAnormal(my.E.data.04, trainingDays=11, dates="2011012401",
                              control=controlBMAnormal(biasCorrection="regression"))
quantileForecast(fit.2004, my.E.data.04, c(.025, .5, .975))
cdf(fit.2004, my.E.data.04, 50)
plot(fit.2004, ensembleData=my.E.data.04, dates="2011012401", ask=FALSE)

############ Now do 2000

### Now use data to make a prediction where:
# (1) The DV is incumbent party's share of 2-party vote
# (2) Training period is 1952-1996
# (3) Prediction is for 2000

in.data.00 <- matrix(NA,  12, 6)
out.data.00 <- matrix(NA, 1, 6)
rownames(in.data.00) <- seq(1952,1997, by=4)
rownames(out.data.00) <- 2000

# Campbell
campbell.in.00 <- campbell.in.08[campbell.in.08$year<=1996,]
campbell.out.00 <- campbell.in.08[campbell.in.08$year==2000,]

c.model <- lm(INPTYVOTE~SEPTPOLL+GDPQTR2HALF, data=campbell.in.00)
summary(c.model)

in.data.00[,1] <- fitted(c.model)
out.data.00[,1] <- predict(c.model, newdata=campbell.out.00)

## Lewis-Beck
lb.in.00 <- lb.in.08[lb.in.08$year<=1996,]
lb.out.00 <- lb.in.08[lb.in.08$year==2000,]

lb.model <- lm(dv ~ JULYPOP + INCXGNP + JOBHOUSU + CLOSEINC, data=lb.in.00)
summary(lb.model)
in.data.00[,2] <- fitted(lb.model)
out.data.00[,2] <- predict(lb.model, newdata=lb.out.00)

colnames(out.data.00) <- colnames(in.data.00) <- c("Campbell", "Lewis-Beck", "Erickson", "Fair", "Hibbs", "Abramowitz")

# Erickson-Wlezien Data

ew.in.00 <- ew.in.08[ew.in.08$year<=1996,]
ew.out.00 <- ew.in.08[ew.in.08$year==2000,]

ew.model <- lm(IncumbentVote ~ l1CumLEIGrowth + IncumbentPoll, data=ew.in.00)
summary(ew.model)

in.data.00[,3] <- fitted(ew.model)
out.data.00[,3] <- predict(ew.model, newdata=ew.out.00)

#### Fair Data
fair.in.00 <- fair.in.08[fair.in.08$year<=1996,]
fair.out.00 <- fair.in.08[fair.in.08$year==2000,]

final.fair.model <- lm(dv ~ G + P + Z + aDPER + aDUR + WAR + newI, data=fair.in.00)
summary(final.fair.model)
in.data.00[,4] <- fitted(final.fair.model)
out.data.00[,4] <- predict(final.fair.model, newdata=fair.out.00)

### Hibbs model
hibbs.in.00 <- hibbs.in.08[hibbs.in.08$year<=1996,]
hibbs.out.00 <- hibbs.in.08[hibbs.in.08$year==2000,]

hibbs.model <- nls(dv ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) + ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) + ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) + ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/(1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 + g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*Fatalities, start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=hibbs.in.00)
summary(hibbs.model)
in.data.00[,5] <- fitted(hibbs.model)
out.data.00[,5] <- predict(hibbs.model, newdata=hibbs.out.00)

### Abramowitz replications
ab.in.00 <- ab.in.08[ab.in.08$year<=1996,]
ab.out.00 <- ab.in.08[ab.in.08$year==2000,]

ab.model <- lm(dv~q2gdp+term+juneapp,data=ab.in.00)
summary(ab.model)
in.data.00[,6] <- fitted(ab.model)
out.data.00[,6] <- predict(ab.model, newdata=ab.out.00)

##### use ebma package

full.forecasts.00 <- rbind(in.data.00, out.data.00)
full.observed.00 <- c(campbell$INPTYVOTE[2:14])

dates <- dates[1:13]

my.E.data.00 <- ensembleData(forecasts=(full.forecasts.00^(1/a)), dates=dates, observations=full.observed.00, initializationTime=1, forecastHour=1)

# 2000 fitting
fit.2000 <- ensembleBMAnormal(my.E.data.00, trainingDays=11, dates="2011012301",
                              control=controlBMAnormal(biasCorrection="regression"))

quantileForecast(fit.2000, my.E.data.00, c(.025, .5, .975))
cdf(fit.2000, my.E.data.00, 50)
plot(fit.2000, ensembleData=my.E.data.00, dates="2011012301", ask=FALSE, x.lab="jacob")

########################################
# Now let's do 1996


### Now use data to make a prediction where:
# (1) The DV is incumbent party's share of 2-party vote
# (2) Training period is 1952-1992
# (3) Prediction is for 1996

in.data.96 <- matrix(NA,  11, 6)
out.data.96 <- matrix(NA, 1, 6)
rownames(in.data.96) <- seq(1952,1997, by=4)
rownames(out.data.96) <- 1996

# Campbell
campbell.in.96 <- campbell.in.08[campbell.in.08$year<=1992,]
campbell.out.96 <- campbell.in.08[campbell.in.08$year==1996,]

c.model <- lm(INPTYVOTE~SEPTPOLL+GDPQTR2HALF, data=campbell.in.96)
summary(c.model)

in.data.96[,1] <- fitted(c.model)
out.data.96[,1] <- predict(c.model, newdata=campbell.out.00)

## Lewis-Beck
lb.in.96 <- lb.in.08[lb.in.08$year<=1992,]
lb.out.96 <- lb.in.08[lb.in.08$year==1996,]

lb.model <- lm(dv ~ JULYPOP + INCXGNP + JOBHOUSU + CLOSEINC, data=lb.in.96)
summary(lb.model)
in.data.96[,2] <- fitted(lb.model)
out.data.96[,2] <- predict(lb.model, newdata=lb.out.96)

colnames(out.data.96) <- colnames(in.data.96) <- c("Campbell", "Lewis-Beck", "Erickson", "Fair", "Hibbs", "Abramowitz")

# Erickson-Wlezien Data

ew.in.96 <- ew.in.08[ew.in.08$year<=1992,]
ew.out.96 <- ew.in.08[ew.in.08$year==1996,]

ew.model <- lm(IncumbentVote ~ l1CumLEIGrowth + IncumbentPoll, data=ew.in.96)
summary(ew.model)

in.data.96[,3] <- fitted(ew.model)
out.data.96[,3] <- predict(ew.model, newdata=ew.out.96)

#### Fair Data
fair.in.96 <- fair.in.08[fair.in.08$year<=1992,]
fair.out.96 <- fair.in.08[fair.in.08$year==1996,]

final.fair.model <- lm(dv ~ G + P + Z + aDPER + aDUR + WAR + newI, data=fair.in.96)
summary(final.fair.model)
in.data.96[,4] <- fitted(final.fair.model)
out.data.96[,4] <- predict(final.fair.model, newdata=fair.out.96)

### Hibbs model
hibbs.in.96 <- hibbs.in.08[hibbs.in.08$year<=1992,]
hibbs.out.96 <- hibbs.in.08[hibbs.in.08$year==1996,]

hibbs.model <- nls(dv ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) + ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) + ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) + ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/(1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 + g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*Fatalities, start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=hibbs.in.96)
summary(hibbs.model)
in.data.96[,5] <- fitted(hibbs.model)
out.data.96[,5] <- predict(hibbs.model, newdata=hibbs.out.96)

### Abramowitz replications
ab.in.96 <- ab.in.08[ab.in.08$year<=1992,]
ab.out.96 <- ab.in.08[ab.in.08$year==1996,]

ab.model <- lm(dv~q2gdp+term+juneapp,data=ab.in.96)
summary(ab.model)
in.data.96[,6] <- fitted(ab.model)
out.data.96[,6] <- predict(ab.model, newdata=ab.out.96)

##### use ebma package

full.forecasts.96 <- rbind(in.data.96, out.data.96)
full.observed.96 <- c(campbell$INPTYVOTE[2:13])

dates <- dates[1:12]

my.E.data.96 <- ensembleData(forecasts=full.forecasts.96^(1/a), dates=dates, observations=full.observed.96, initializationTime=1, forecastHour=1)

# 1996 fitting
fit.1996<- ensembleBMAnormal(my.E.data.96, trainingDays=11, dates="2011012201",
                              control=controlBMAnormal(biasCorrection="regression"))

quantileForecast(fit.1996, my.E.data.96, c(.025, .5, .975))
cdf(fit.1996, my.E.data.96, 50)
plot(fit.1996, ensembleData=my.E.data.96, dates="2011012201", ask=FALSE, x.lab="jacob")



## all together
par(mfrow=c(3,1), xaxt="n")
plot(fit.2000, ensembleData=my.E.data.00, dates="2011012301", ask=FALSE, xlab="jacob")
plot(fit.2004, ensembleData=my.E.data.04, dates="2011012401", ask=FALSE, xlab="jacob")
jacob <- plot(fit.2008, ensembleData=my.E.data.08, dates="2011012501", ask=FALSE, xlab="jacob")


err.1996 <- c(out.data.96, quantileForecast(fit.1996, my.E.data.96))-campbell$INPTYVOTE[13]
err.2000 <- c(out.data.00, quantileForecast(fit.2000, my.E.data.00))-campbell$INPTYVOTE[14]
err.2004 <- c(out.data.04, quantileForecast(fit.2004, my.E.data.04)) -campbell$INPTYVOTE[15]
err.2008 <- c(out.data, quantileForecast(fit.2008, my.E.data.08))-campbell$INPTYVOTE[16]



####trying out fitting using pure out-of-sample predictions


out.samp.pred <- matrix(NA, nrow=9, ncol=6)

for (tyn in 7:15){
   master.years <- seq(1952, 2008, by=4)
   in.data <- matrix(NA, (tyn-1), 6)
   out.data <- matrix(NA, 1, 6)
   rownames(in.data) <- master.years[1:(tyn-1)]
   rownames(out.data) <- master.years[tyn]
   colnames(out.data) <- colnames(in.data) <- c("Campbell", "Lewis-Beck", "Erickson", "Fair", "Hibbs", "Abramowitz")
   in.master <- master.data[master.data$year >=1952 & master.data$year<master.years[tyn],]
   out.master <- master.data[master.data$year == master.years[tyn],]
   c.model <- lm(dv~septpoll+gdpqtr2half, data=master.data[master.data$year<master.years[tyn],])
   in.data[,1] <- predict(c.model, newdata=in.master)
   out.data[,1] <- predict(c.model, newdata=out.master)
   lb.model <- lm(dv ~ julypop + incxgnp + jobhousu + closeinc, data=master.data[master.data$year<master.years[tyn],])
   in.data[,2] <- predict(lb.model, newdata=in.master)
   out.data[,2] <- predict(lb.model, newdata=out.master)
   ew.model <- lm(dv ~ l1cumleigrowth + incumbentpoll, data=master.data[master.data$year<master.years[tyn],])
   in.data[,3] <- predict(ew.model, newdata=in.master)
   out.data[,3] <- predict(ew.model, newdata=out.master)
   fair.model <- lm(dv ~ G + P + Z + adper + adur + war + I, data=master.data[master.data$year<master.years[tyn],])
   in.data[,4] <- predict(fair.model, newdata=in.master)
   out.data[,4] <- predict(fair.model, newdata=out.master)
   hibbs.model <- nls(dv ~ beta0 + bdlnr* (((1.0*wtq16*dnlr) + (g*dnlr.L1) + ((g^2)*dnlr.L2) + ((g^3)*dnlr.L3) +
                                             ((g^4)*dnlr.L4) + ((g^5)*dnlr.L5) + ((g^6)*dnlr.L6) + ((g^7)*dnlr.L7) +
                                             ((g^8)*dnlr.L8) + ((g^9)*dnlr.L9) + ((g^10)*dnlr.L10) + ((g^11)*dnlr.L11) +
                                             ((g^12)*dnlr.L12)+ ((g^13)*dnlr.L13)+ ((g^14)*dnlr.L14))/
                                            (1.0*wtq16 + g + g^2 + g^3 + g^3 + g^4 + g^5 + g^6 + g^7 + g^8 + g^9 +
                                             g^10 + g^11 + g^12 + g^13 + g^14)) + bkia*fatalities,
                       start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1), data=master.data[master.data$year<master.years[tyn],])
   in.data[,5] <- predict(hibbs.model, newdata=in.master)
   out.data[,5] <- predict(hibbs.model, newdata=out.master)
   ab.model <- lm(dv~q2gdp+term+juneapp,data=master.data[master.data$year<master.years[tyn],])
   in.data[,6] <- predict(ab.model, newdata=in.master)
   out.data[,6] <- predict(ab.model, newdata=out.master)
   out.samp.pred[tyn-6,] <- out.data
 }
 colnames(out.samp.pred) <- c("Campbell", "Lewis-Beck", "Erickson", "Fair", "Hibbs", "Abramowitz")
 rownames(out.samp.pred) <- c(seq(from=1976, to=2008, by=4)
   
   # Now fit the ebma model
    full.forecasts <- rbind(in.data, out.data)
    full.observed <- c(master.data$dv[10:(9+tyn)])
