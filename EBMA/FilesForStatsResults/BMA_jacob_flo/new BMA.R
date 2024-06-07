


library(ICEWS2)
demo(ICEWS2) #demo(ICESW2) runs the a demo of the models- i use this to get the results from the duke predictions


#each of the inpred does the same for all dependent variables, so I will comment only on the rebell one
inpred<-data.frame(model.rebellion$trainingset$country,model.rebellion$trainingset$iso,model.rebellion$trainingset$year,model.rebellion$trainingset$month,model.rebellion$trainingset$rebellion,model.rebellion$in.pred)
names(inpred)<-c("Country","ISO","Year","Month","Truth","Pred")
#above paragraph creates dataframe with all the countries, iso-code, month, truth data, and prediction for the trainingset
outpred<-data.frame(model.rebellion$testset$country,model.rebellion$testset$iso,model.rebellion$testset$year,model.rebellion$testset$month,model.rebellion$testset$rebellion,model.rebellion$out.pred)
names(outpred)<-c("Country","ISO","Year","Month","Truth","Pred")
#above paragraph does the same for the testset- i guess ward wants an indicator function to determine testset/tainingset? right now this the icews model has the data stored in $trainingset and $testset, as are the predictions
predreb<-rbind(inpred,outpred)
#combine the testset and trainingset dataframes
attach(predreb)
pred.reb <- predreb[order(Year, Month,Country),]
#oder by year, month, county, so that we can merge the data later with the other model results
detach(predreb)

#same thing as above for the other four dependent variables follows in the next four parts
inpred1<-data.frame(model.insurgency$trainingset$country,model.insurgency$trainingset$iso,model.insurgency$trainingset$year,model.insurgency$trainingset$month,model.insurgency$trainingset$insurgency,model.insurgency$in.pred)
names(inpred1)<-c("Country","ISO","Year","Month","Truth","Pred")
outpred1<-data.frame(model.insurgency$testset$country,model.insurgency$testset$iso,model.insurgency$testset$year,model.insurgency$testset$month,model.insurgency$testset$insurgency,model.insurgency$out.pred)
names(outpred1)<-c("Country","ISO","Year","Month","Truth","Pred")
predinsur<-rbind(inpred1,outpred1)
attach(predinsur)
pred.insur <- predinsur[order(Year, Month,Country),]
detach(predinsur)


inpred2<-data.frame(model.crisisdomestic$trainingset$country,model.crisisdomestic$trainingset$iso,model.crisisdomestic$trainingset$year, model.crisisdomestic$trainingset$month, model.crisisdomestic$trainingset$crisisdomestic, model.crisisdomestic$in.pred)
names(inpred2)<-c("Country","ISO","Year","Month","Truth","Pred")
outpred2<-data.frame(model.crisisdomestic$testset$country,model.crisisdomestic$testset$iso,model.crisisdomestic$testset$year, model.crisisdomestic$testset$month, model.crisisdomestic$testset$crisisdomestic, model.crisisdomestic$out.pred)
names(outpred2)<-c("Country","ISO","Year","Month","Truth","Pred")
predcrisisdom<-rbind(inpred2,outpred2)
attach(predcrisisdom)
pred.crisisdom<- predcrisisdom[order(Year, Month,Country),]
detach(predcrisisdom)

inpred3<-data.frame(model.crisisintl$trainingset$country,model.crisisintl$trainingset$iso, model.crisisintl$trainingset$year, model.crisisintl$trainingset$month, model.crisisintl$trainingset$crisisintl, model.crisisintl$in.pred)
names(inpred3)<-c("Country","Year","Month","Truth","Pred")
outpred3<-data.frame(model.crisisintl$testset$country,model.crisisintl$testset$iso,model.crisisintl$testset$year, model.crisisintl$testset$month, model.crisisintl$testset$crisisintl, model.crisisintl$out.pred)
names(outpred3)<-c("Country","ISO","Year","Month","Truth","Pred")
predcrisisintl<-rbind(inpred3,outpred3)
attach(predcrisisintl)
pred.crisisintl<- predcrisisintl[order(Year, Month,Country),]
detach(predcrisisintl)


inpred4<-data.frame(model.violence$trainingset$country,model.violence$trainingset$iso, model.violence$trainingset$year, model.violence$trainingset$month, model.violence$trainingset$violence, model.violence$in.pred)
names(inpred4)<-c("Country","ISO","Year","Month","Truth","Pred")
outpred4<-data.frame(model.violence$testset$country,model.violence$testset$iso,model.violence$testset$year, model.violence$testset$month, model.violence$testset$violence, model.violence$out.pred)
names(outpred4)<-c("Country","ISO","Year","Month","Truth","Pred")
predviolence<-rbind(inpred4,outpred4)
attach(predviolence)
pred.violence<- predviolence[order(Year, Month,Country),]
detach(predviolence)



library(foreign)
##still have to do the path to shared drive and set as wd, will do so soon
wd<-c("/Users/florianhollenbach/Documents/Duke/Fall2010/RA/new BMA/Insurgency model for Mike Ward 20100927")
setwd(wd)
data <- read.dta("ICEWS2.build3.month.16sep2010.dta")
#read in data from SAE, the following is their code, to run the model
data <- subset(data, year>1997)

date <- paste(data$year, data$month, 1, sep="/")
date <- strptime(date, format="%Y/%m/%d")
data$date <- date
rm(date)



ins <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence + L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals +  L6Ginsurgentshosttotals + L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 + L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals + L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate + L12ethnfracstate2 +  L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 + L12imp_relfracstate + L12imp_relfracstate2,
	data = data, family=binomial(link="logit"), na.action="na.exclude")

#they then create a spreadsheet with Country, year, month, thruth, sae
data$pred.probabilities <- predict(ins, newdata = data, type="response")
out <- subset(data, select=c(ccode,country, year, month, GTDSinsurgency, pred.probabilities))
names(out)<-c("Country","Year","Month", "Truth","SAE")
write.table(out, file="insurgency.results.txt", sep="\t", col.names=TRUE, row.names=FALSE, append=FALSE)


#the next three lines are my meager first try at combining data from our model with their results, we have to change some country names in SEA data, to make them matching

attach(ou1)
blub12<-ou1[order(Year,Month,Country),]
Insurgency.pred<-cbind(pred.insur1,blub12$SAE)
blub12$SAE

##############
############# THE REST OF THE CODE IS EVERTHING I DID FOR THE BMA MODELS FOR MIKE FOR ONE DEPENDENT VARIABLE (here Rebellion)
############## the input previously came from .csv files he gave me, i had two seperate rcode files for every variable
############## one file for the BMA, one for density plots, he wrote the original code, i rewrote it for each variable
###############

# ensemble BMA test run
# mdw, june 29, 2010
library(foreign)
library(ensembleBMA)
#### again i'll figure out how to change the wd to the share drive

dd<-c("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/data")
od<-c("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/output")
setwd(dd)


dat<-read.csv(header=T,"/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/data/Rebellion.csv")
### read in the csv file with predictions and truth
dat1<-data.frame(dat[1:4351,] ) # remove cases with NA on gtds/need to be changed?
dat1<-dat1[-4351,]
library(BMA)


x<-dat[,c(3,5,6,7)]
y<-dat[,8]
#assign predictions to x, and truth data to y
bma.fit<- bic.glm(x, y, strict = FALSE, OR = 20, glm.family="binomial", 
    factor.type=TRUE)
###fit the bma model    
    
    
plot(density(bma.fit$postprob))    
# plot posterior probability 
summary(bma.fit)

imageplot.bma(bma.fit)

b0<-bma.fit$postmean[1]
b1<-bma.fit$postmean[2]*bma.fit$x[,1]
b2<-bma.fit$postmean[3]*bma.fit$x[,2]
b3<-bma.fit$postmean[4]*bma.fit$x[,3]
eta<-b0 + b1 + b2 + b3
pred<-(exp(eta))/(1+ exp(eta))
### i am not a %100 sure what is done above, but i believe the bma predictions are calcuated 

pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/BMAdensity2.pdf",
    width=5, height=5)
    
plot(density(pred,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="black",main="")

dev.off() 
##### plot predicted probabilities of the bma 

#### next up is the code to create different densities of the predictions, those went into the pdf files, Mike wants them to be created in the wrapper as well, I sent him an email


pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityA.pdf",
    width=5, height=5)    
plot(density(dat$duke,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="blue",ylim=c(0,80),main="")
lines(density(dat$psu,na.rm=T),lwd=2,col="red")
lines(density(dat$sae,na.rm=T),lwd=2,col="purple")
lines(density(dat$lustik,na.rm=T),lwd=2,col="green")
lines(density(dat$idi,na.rm=T),lwd=2,col="orange")
lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("blue","red","purple","green","orange","black")
legend(.6, 75, c("Duke","PSU","SAE","Lustik","IDI","Truth"),
       text.col=mycol,bty="n")
dev.off()    
######## above plot is created of the predicted probabilities from each model and the ground truth data


pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityb.pdf",
    width=5, height=5)    
plot(density(dat$duke,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="blue",ylim=c(0,20),main="")
lines(density(dat$psu,na.rm=T),lwd=2,col="red")
lines(density(dat$sae,na.rm=T),lwd=2,col="purple")
lines(density(dat$lustik,na.rm=T),lwd=2,col="green")
#lines(density(dat$idi,na.rm=T),lwd=2,col="orange")
lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("blue","red","purple","green","black")
legend(.6, 20, c("Duke","PSU","SAE","Lustik","Truth"),
       text.col=mycol,bty="n")
dev.off() 
##### same as above, but lustik excluded



pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityc.pdf",
    width=5, height=5)
plot(density(dat$lustik,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="green",ylim=c(0,5),main="")
lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("green","black")
legend(.6, 4, c("Lustik","Truth"),
       text.col=mycol,bty="n")
dev.off() 
###only lustik and truth


