library(ICEWS2)
demo(ICEWS2)

inpred<-data.frame(model.rebellion$trainingset$country,model.rebellion$trainingset$year,model.rebellion$trainingset$month,model.rebellion$trainingset$rebellion,model.rebellion$in.pred)
names(inpred)<-c("Country","Year","Month","Truth","Pred")
outpred<-data.frame(model.rebellion$testset$country,model.rebellion$testset$year,model.rebellion$testset$month,model.rebellion$testset$rebellion,model.rebellion$out.pred)
names(outpred)<-c("Country","Year","Month","Truth","Pred")
predreb<-rbind(inpred,outpred)
attach(predreb)
predreb
pred.reb <- predreb[order(Year, Month,Country),]
detach(predreb)

inpred1<-data.frame(model.insurgency$trainingset$country,model.insurgency$trainingset$year,model.insurgency$trainingset$month,model.insurgency$trainingset$insurgency,model.insurgency$in.pred)
names(inpred1)<-c("Country","Year","Month","Truth","Pred")
outpred1<-data.frame(model.insurgency$testset$country,model.insurgency$testset$year,model.insurgency$testset$month,model.insurgency$testset$insurgency,model.insurgency$out.pred)
names(outpred1)<-c("Country","Year","Month","Truth","Pred")
predinsur<-rbind(inpred1,outpred1)
attach(predinsur)
pred.insur <- predinsur[order(Year, Month,Country),]
detach(predinsur)


inpred2<-data.frame(model.crisisdomestic$trainingset$country, model.crisisdomestic$trainingset$year, model.crisisdomestic$trainingset$month, model.crisisdomestic$trainingset$crisisdomestic, model.crisisdomestic$in.pred)
names(inpred2)<-c("Country","Year","Month","Truth","Pred")
outpred2<-data.frame(model.crisisdomestic$testset$country, model.crisisdomestic$testset$year, model.crisisdomestic$testset$month, model.crisisdomestic$testset$crisisdomestic, model.crisisdomestic$out.pred)
names(outpred2)<-c("Country","Year","Month","Truth","Pred")
predcrisisdom<-rbind(inpred2,outpred2)
attach(predcrisisdom)
pred.crisisdom<- predcrisisdom[order(Year, Month,Country),]
detach(predcrisisdom)

inpred3<-data.frame(model.crisisintl$trainingset$country, model.crisisintl$trainingset$year, model.crisisintl$trainingset$month, model.crisisintl$trainingset$crisisintl, model.crisisintl$in.pred)
names(inpred3)<-c("Country","Year","Month","Truth","Pred")
outpred3<-data.frame(model.crisisintl$testset$country, model.crisisintl$testset$year, model.crisisintl$testset$month, model.crisisintl$testset$crisisintl, model.crisisintl$out.pred)
names(outpred3)<-c("Country","Year","Month","Truth","Pred")
predcrisisintl<-rbind(inpred3,outpred3)
attach(predcrisisintl)
pred.crisisintl<- predcrisisintl[order(Year, Month,Country),]
detach(predcrisisintl)


inpred4<-data.frame(model.violence$trainingset$country, model.violence$trainingset$year, model.violence$trainingset$month, model.violence$trainingset$violence, model.violence$in.pred)
names(inpred4)<-c("Country","Year","Month","Truth","Pred")
outpred4<-data.frame(model.violence$testset$country, model.violence$testset$year, model.violence$testset$month, model.violence$testset$violence, model.violence$out.pred)
names(outpred4)<-c("Country","Year","Month","Truth","Pred")
predviolence<-rbind(inpred4,outpred4)
attach(predviolence)
pred.violence<- predviolence[order(Year, Month,Country),]
detach(predviolence)


library(foreign)

wd<-c("/Users/florianhollenbach/Documents/Duke/Fall2010/RA/new BMA/Insurgency model for Mike Ward 20100927")
library(foreign)
setwd(wd)
data <- read.dta("ICEWS2.build3.month.16sep2010.dta")

data <- subset(data, year>1998)

date <- paste(data$year, data$month, 1, sep="/")
date <- strptime(date, format="%Y/%m/%d")
data$date <- date
rm(date)



ins <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence + L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals +  L6Ginsurgentshosttotals + L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 + L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals + L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate + L12ethnfracstate2 +  L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 + L12imp_relfracstate + L12imp_relfracstate2,
	data = data, family=binomial(link="logit"), na.action="na.exclude")

data$pred.probabilities <- predict(ins, newdata = data, type="response")
out <- subset(data, year > 1998, select=c(country, year, month, pred.probabilities))
names(out)<-c("Country","Year","Month","SAE")
ou1<-subset(out, year<2011, select=c(Country,Year,Month,SAE))
attach(ou1)
blub12<-ou1[order(Year,Month,Country),]
Insurgency.pred<-cbind(pred.insur1,blub12$SAE)
blub12$SAE



write.table(out, file="insurgency.results.txt", sep="\t", col.names=TRUE, row.names=FALSE, append=FALSE)

pred.insurgency1<-cbind(pred.insur,)