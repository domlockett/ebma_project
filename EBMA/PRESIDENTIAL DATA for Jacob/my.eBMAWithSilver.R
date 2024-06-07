

my.eBMA <- function(tyn = 15, a = 3, train.years =  10, hibbpreds=hibbspreds){

   master.years <- seq(1952, 2008, by=4)
   in.data <- matrix(NA, (tyn-1), 7)
   out.data <- matrix(NA, 1, 7)
   rownames(in.data) <- master.years[1:(tyn-1)]
   rownames(out.data) <- master.years[tyn]
   colnames(out.data) <- colnames(in.data) <- c("Campbell", "Lewis-Beck", "Erikson", "Fair", "Hibbs", "Abramowitz", "Silver")
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
                       start=list(beta0=45, g=0.95, bdlnr=4, bkia=-0.1),
                      data=master.data[master.data$year<master.years[tyn],])
   in.data[,5] <- predict(hibbs.model, newdata=in.master)
   out.data[,5] <- predict(hibbs.model, newdata=out.master)
   
   ab.model <- lm(dv~q2gdp+term+juneapp,data=master.data[master.data$year<master.years[tyn],])
   in.data[,6] <- predict(ab.model, newdata=in.master)
   out.data[,6] <- predict(ab.model, newdata=out.master)

   s.model <- lm(dv~approvals+ideologys+gdps, data=master.data[master.data$year<master.years[tyn],])
   in.data[,7]<-predict(s.model, newdata=in.master)
   out.data[,7]<-predict(s.model, newdata=out.master)


   
    # record the predictive inverals
   ninefive <- matrix(nrow=7, ncol=2)
   ninefive[1,]=predict(c.model, newdata=out.master, interval="prediction", level=.95)[2:3]
   ninefive[2,]=predict(lb.model, newdata=out.master, interval="prediction", level=.95)[2:3]
   ninefive[3,]=predict(ew.model, newdata=out.master, interval="prediction", level=.95)[2:3]
   ninefive[4,]=predict(fair.model, newdata=out.master, interval="prediction", level=.95)[2:3]
   ninefive[5,]=c(hibbspreds$X95_low[hibbspreds$Year==master.years[tyn]], hibbspreds$X95_high[hibbspreds$Year==master.years[tyn]]) 
   ninefive[6,]=predict(ab.model, newdata=out.master, interval="prediction", level=.95)[2:3]
   ninefive[7,]=predict(s.model, newdata=out.master, interval="prediction", level=.95)[2:3]

   sixseven <- matrix(nrow=7, ncol=2)
   sixseven[1,]=predict(c.model, newdata=out.master, interval="prediction", level=.67)[2:3]
   sixseven[2,]=predict(lb.model, newdata=out.master, interval="prediction", level=.67)[2:3]
   sixseven[3,]=predict(ew.model, newdata=out.master, interval="prediction", level=.67)[2:3]
   sixseven[4,]=predict(fair.model, newdata=out.master, interval="prediction", level=.67)[2:3]
   sixseven[5,]=c(hibbspreds$X66_low[hibbspreds$Year==master.years[tyn]], hibbspreds$X66_high[hibbspreds$Year==master.years[tyn]]) 
   sixseven[6,]=predict(ab.model, newdata=out.master, interval="prediction", level=.67)[2:3]
   sixseven[7,]=predict(s.model, newdata=out.master, interval="prediction", level=.67)[2:3]

   ninezero <- matrix(nrow=7, ncol=2)
   ninezero[1,]=predict(c.model, newdata=out.master, interval="prediction", level=.90)[2:3]
   ninezero[2,]=predict(lb.model, newdata=out.master, interval="prediction", level=.90)[2:3]
   ninezero[3,]=predict(ew.model, newdata=out.master, interval="prediction", level=.90)[2:3]
   ninezero[4,]=predict(fair.model, newdata=out.master, interval="prediction", level=.90)[2:3]
   ninezero[5,]=c(hibbspreds$X90_low[hibbspreds$Year==master.years[tyn]], hibbspreds$X90_high[hibbspreds$Year==master.years[tyn]]) 
   ninezero[6,]=predict(ab.model, newdata=out.master, interval="prediction", level=.90)[2:3]
   ninezero[7,]=predict(s.model, newdata=out.master, interval="prediction", level=.90)[2:3]

   # Now fit the ebma model
    full.forecasts <- rbind(in.data, out.data)
    full.observed <- c(master.data$dv[10:(9+tyn)])

   # Stupid thing to make the ebma forecast work with the rest
   dates <- rep(NA, tyn)
   for (i in 1:tyn){
     dates[i] <- paste("2011", "01", 10+i, "01", sep="")
    }

   pred.date <- dates[tyn]

   ## Assemble everything needed to make a full EBMA forecast
   my.E.data <- ensembleData(forecasts=(full.forecasts)^(1/a), dates=dates, observations=full.observed,
                             initializationTime=1, forecastHour=1)

   ## Fit the EBMA model
   fit.eBMA <- ensembleBMAnormal(my.E.data, trainingDays=train.years, dates=pred.date, minCRPS=TRUE,
                              control=controlBMAnormal(biasCorrection="none"))
   
   # Make the prediction for the next year
   conf.int <- quantileForecast(fit.eBMA, my.E.data, c(.025, .05, .166666, .5, .833333, .95, .975))

   # Put together everything that is needed for output
   cdf.5 <- cdf(fit.eBMA, my.E.data, 50)
   err <- cbind(out.data-full.observed[tyn], conf.int[4]-full.observed[tyn])
   colnames(err) <- c(colnames(in.data), "EBMA")
   observed <- full.observed[tyn]
   cov.95 <- c(observed > ninefive[,1] & observed < ninefive[,2],
                   observed<=conf.int[7] & observed>=conf.int[1])
   names(cov.95) <- c(colnames(in.data), "EBMA")
   cov.90 <- c(observed > ninezero[,1] & observed < ninezero[,2],
                   observed<=conf.int[6] & observed>=conf.int[2])
   names(cov.90) <- c(colnames(in.data), "EBMA")
   cov.67 <- c(observed > sixseven[,1] & observed < sixseven[,2],
                   observed<=conf.int[5] & observed>=conf.int[3])
   names(cov.67) <- c(colnames(in.data), "EBMA")
    this.crps <- crps(fit.eBMA, my.E.data)

   
   out <- list(in.data=in.data, out.data=out.data, full.observed=full.observed, pred.date=pred.date,
               E.data=my.E.data, fit.eBMA=fit.eBMA, conf.int=conf.int, cdf.5 =cdf.5, err=err,
               observed=full.observed[tyn], cov.67=cov.67, cov.95=cov.95, this.crps=this.crps,
               ninefive=ninefive, sixseven=sixseven, ninezero=ninezero, cov.90=cov.90
               )
   out
 }
