# Include the "dumb average" 
# glm in the paper is glm.simple in the r file
# LMER in the paper is model.lmer.3 in the r-file
# SAE is SAE


library(ICEWS2)
#icews <- read.csv("dataICEWS.csv")
data(icews)


# Make the three sets
icews$valid.choose<-rbinom(4872, prob=.1, size=1)
#& icews$valid.choose == 0
training.sample <- icews[icews$year>=1999 & icews$year<=2007,] #1996-2005
validation.sample <- icews[icews$year>=2008 & icews$year<=2009,] #2006-2008
test.sample <- icews[icews$year>=2010 & icews$year<=2010,] #2009-2010

for.SAE <- data.frame(icews$valid.choose, as.character(icews$country), icews$year, icews$month)1
names(for.SAE) <- c("valid.choose", "Country", "year", "month")

library(foreign)
SAE.data <- read.dta("ICEWS2.build3.month.16sep2010.dta")
dim(SAE.data)
SAE.data$Country<-toupper(SAE.data$country)
for.SAE$Country<-toupper(for.SAE$Country)
my.replace.fun<-function(BAD, GOOD){
  SAE.data$Country[SAE.data$Country ==BAD]<<-GOOD
  for.SAE$Country[for.SAE$Country ==BAD]<<-GOOD
 }
my.replace.fun("NEW_ZEALAND", "NEW ZEALAND")
my.replace.fun("NORTH_KOREA", "NORTH KOREA")
my.replace.fun("PAPUA_NEW_GUINEA", "PAPUA NEW GUINEA")
my.replace.fun("SOLOMIN_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMIN IS.", "SOLOMIN ISLANDS")
my.replace.fun("SOUTH_KOREA", "SOUTH KOREA")
my.replace.fun("SRI_LANKA", "SRI LANKA")
my.replace.fun("BURMA", "MYANMAR")
my.replace.fun("SOLOMON_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMON IS.", "SOLOMIN ISLANDS")
SAE.data <- merge(SAE.data, for.SAE, by=c("Country", "year", "month"))

SAE.training.sample<-SAE.data[ SAE.data$year>=1997 & SAE.data$year<=2009 ,] # 1997-2004
SAE.valid.sample<-SAE.data[SAE.data$year>=2008 & SAE.data$year<=2009,] #2005-2008
SAE.test.sample<-SAE.data[SAE.data$year>=2010 & SAE.data$year<=2010,] #2005-2008



model.glm.simple<-glm(insurgency~  gdpgrowth.l3 +pop.l3 + nminorities + anoc , family="binomial",data=training.sample)
summary(model.glm.simple)
training.glm <- fitted(model.glm.simple)
validation.glm <- predict(model.glm.simple, type="response", newdata=validation.sample)
test.glm <- predict(model.glm.simple, type="response", newdata=test.sample)



model.lmer<-lmer(insurgency~recentonsets.insurgency + lastelection +mil.conf.l3 + W.bl.std.crisisdomestic.l3 + (gdppc.l3 + USAconf.l3|country), 
                      family="binomial", data=training.sample)
summary(model.lmer)
training.lmer <- fitted(model.lmer)
validation.lmer <- predict.lmer(model.lmer, newdata=validation.sample)
test.lmer <- predict.lmer(model.lmer, newdata=test.sample)

###### SAE model
SAE.model <- glm(GTDSinsurgency ~ L6muslimsScooptotals + L6allmuslimsSviolence + L6Sallmuslimsviolence +
           L6separatistsAllhosttotals + L6JIAllhosttotals + L6GDhosttotals + L6Ginsurgentshosttotals +
           L6Ginsurgentshosttotals2 + L6intrapolpartycooptotals + L6intrapolpartycooptotals2 +
           L6orgmuslimsAllhosttotals + L6orgmuslimsAllhosttotals2 + L6insurgentsAllhosttotals +
           L6insurgentsAllhosttotals2 + L12margroups + L12state + L12state2 + L12ethnfrac + L12ethnfracstate +
           L12ethnfracstate2 + L12imp_GDPcurUS + L12imp_GDPcurUS2 + L12lnpop + L12imp_relfrac + L12imp_relfrac2 +
           L12imp_relfracstate + L12imp_relfracstate2,
           data = SAE.training.sample, family=binomial(link="logit"), na.action="na.exclude")
summary(SAE.model)
training.SAE <- fitted(SAE.model)
valid.SAE <- predict(SAE.model, newdata = SAE.valid.sample, type="response")
test.SAE <- predict(SAE.model, newdata = SAE.test.sample, type="response")




# Make Duke datasets
Duke.in.pred <- data.frame(validation.glm, validation.lmer, as.character(validation.sample$country), validation.sample$year, validation.sample$month, validation.sample$insurgency, stringsAsFactors=FALSE)
names(Duke.in.pred)<-c("glm", "lmer",  "Country", "Year", "Month", "Insurgency")
Duke.out.pred <- data.frame(test.glm, test.lmer, as.character(test.sample$country), test.sample$year, test.sample$month, test.sample$insurgency, stringsAsFactors=FALSE)
names(Duke.out.pred)<-c("glm", "lmer", "Country", "Year", "Month", "Insurgency")



# Make SAE data frames
SAE.in.pred <- data.frame(valid.SAE, SAE.valid.sample$country, SAE.valid.sample$year, SAE.valid.sample$month, stringsAsFactors=FALSE)
names(SAE.in.pred) <- c("SAE", "Country", "Year", "Month")
SAE.out.pred <- data.frame(test.SAE, SAE.test.sample$country, SAE.test.sample$year, SAE.test.sample$month, stringsAsFactors=FALSE)
names(SAE.out.pred) <- c("SAE", "Country", "Year", "Month")


# Merge them together
SAE.in.pred$Country<-toupper(SAE.in.pred$Country)
Duke.in.pred$Country<-toupper(Duke.in.pred$Country)
SAE.out.pred$Country<-toupper(SAE.out.pred$Country)
Duke.out.pred$Country<-toupper(Duke.out.pred$Country)
my.replace.fun<-function(BAD, GOOD){
  SAE.in.pred$Country[SAE.in.pred$Country ==BAD]<<-GOOD
  SAE.out.pred$Country[SAE.out.pred$Country ==BAD]<<-GOOD
  Duke.in.pred$Country[Duke.in.pred$Country ==BAD]<<-GOOD
  Duke.out.pred$Country[Duke.out.pred$Country ==BAD]<<-GOOD
 }
my.replace.fun("NEW_ZEALAND", "NEW ZEALAND")
my.replace.fun("NORTH_KOREA", "NORTH KOREA")
my.replace.fun("PAPUA_NEW_GUINEA", "PAPUA NEW GUINEA")
my.replace.fun("SOLOMIN_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMIN IS.", "SOLOMIN ISLANDS")
my.replace.fun("SOUTH_KOREA", "SOUTH KOREA")
my.replace.fun("SRI_LANKA", "SRI LANKA")
my.replace.fun("BURMA", "MYANMAR")
my.replace.fun("SOLOMON_ISLANDS", "SOLOMIN ISLANDS")
my.replace.fun("SOLOMON IS.", "SOLOMIN ISLANDS")
Total.in.pred <- merge(Duke.in.pred, SAE.in.pred, by=c("Country", "Year", "Month"))
Total.out.pred <- merge(Duke.out.pred, SAE.out.pred, by=c("Country", "Year", "Month"))

#### Output to memory
#setwd("/Volumes/ICEWS Project/BMA Stuffs/")
write.csv(Total.in.pred, file="Insample.csv", row.names=FALSE)
write.csv(Total.out.pred, file="Outsample.csv", row.names=FALSE)
# output for the package
rm(list=ls())
# need to change this to be a read in
Insample<-read.csv(file="Insample.csv")
Outsample <- read.csv(file="Outsample.csv")

library(EBMAforecast)
library(separationplot)
my.pred <- cbind(Insample$lmer, Insample$SAE , Insample$glm)
colnames(my.pred) <- c("LMER", "SAE" , "GLM")
my.y <- as.vector(cbind(Insample$Insurgency))

EBMA.fit <- Ensemble.logit(y=my.y, pp.raw=my.pred, tol=.0001, exp=3)
print.Ensemble.logit(EBMA.fit)
plot.Ensemble.logit(EBMA.fit)

my.pred.out <- cbind(Outsample$lmer, Outsample$SAE , Outsample$glm)
my.y.out <- as.vector(cbind(Outsample$Insurgency))

EBMA.pred <- predict.Ensemble.logit(obj=EBMA.fit, newdata=my.pred.out, y.out=my.y.out)
print.Ensemble.logit(EBMA.pred)
plot.Ensemble.logit(EBMA.pred)


table(my.y)/sum(table(my.y))
table(my.y.out)/sum(table(my.y.out))


### compare with average

average <- rowMeans(my.pred)
my.pred.av <-  cbind(my.pred, average)
colnames(my.pred.av) <- c("LMER", "SAE", "GLM", "AVE")
EBMA.fit.av <- Ensemble.logit(y=my.y, pp.raw=my.pred.av, tol=.001, exp=3)
print.Ensemble.logit(EBMA.fit.av)
my.pred.out.av <- cbind(my.pred.out, rowMeans(my.pred.out))

EBMA.pred <- predict.Ensemble.logit(obj=EBMA.fit.av, newdata=my.pred.out.av, y.out=my.y.out)
print.Ensemble.logit(EBMA.fit.av)
