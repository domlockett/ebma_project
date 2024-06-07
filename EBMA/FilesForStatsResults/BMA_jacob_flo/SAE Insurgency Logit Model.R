# altered by Jacob on 3-3-2011

library(foreign)
setwd("/Volumes/ICEWS Project/BMA Stuffs/BMA_jacob_flo/")
SAE.data <- read.dta("ICEWS2.build3.month.16sep2010.dta")

#data <- subset(data, year>1998)

#date <- paste(data$year, data$month, 1, sep="/")
#date <- strptime(date, format="%Y/%m/%d")
#data$date <- date
#rm(date)

SAE.in.sample<-SAE.data[SAE.data$year<=2008 & SAE.data$year>=1998,]
SAE.out.sample<-SAE.data[SAE.data$year==2009 | SAE.data$year==2010 ,]

SAE.model <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence +
           L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals + L6Ginsurgentshosttotals +
           L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 +
           L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals +
           L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate +
           L12ethnfracstate2 + L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 +
           L12imp_relfracstate + L12imp_relfracstate2,
           data = SAE.in.sample, family=binomial(link="logit"), na.action="na.exclude")

in.SAE <- fitted(SAE.model)

out.SAE <- predict(SAE.model, newdata = SAE.out.sample, type="response")

#out <- subset(out, select=c(country, year, month, pred.probabilities))
#write.table(out, file="insurgency.results.txt", sep="\t", col.names=TRUE, row.names=FALSE, append=FALSE)







#Above code works fine.


ins <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence + L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals +  L6Ginsurgentshosttotals + L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 + L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals + L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate + L12ethnfracstate2 +  L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 + L12imp_relfracstate + L12imp_relfracstate2,
	data = data, family=binomial(link="logit"), na.action="na.exclude")

data$pred.probabilities <- predict(ins, newdata = data, type="response")
out <- subset(data, year > 1998, select=c(country, year, month, pred.probabilities))

write.table(out, file="insurgency.results.txt", sep="\t", col.names=TRUE, row.names=FALSE, append=FALSE)
