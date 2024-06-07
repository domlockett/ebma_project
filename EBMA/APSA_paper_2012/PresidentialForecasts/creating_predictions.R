library(foreign)
setwd("~/Dropbox/EBMA/APSA_paper_2012/PresidentialForecasts/")
holbrook<-read.dta("~/Dropbox/EBMA/APSA_paper_2012/PresidentialForecasts/Holbrook_data_for_Ward.dta")
data.holbrook<-subset(holbrook,year<2012)
head(holbrook)

model.holbrook<-lm(vote~conditions+openseat+conditions*openseat,data=data.holbrook)
insample.holbrook<-fitted(model.holbrook)
years<-data.holbrook$year
insample.holbrook<-data.frame(years,insample.holbrook)
names(insample.holbrook)<-c("Year","Holbrook")

data.campbel<-read.csv("~/Dropbox/EBMA/APSA_paper_2012/PresidentialForecasts/Campbel_data.csv")
head(data.campbel)
insample.campbel.trialheat<-data.campbel[,c("YEAR","PREDICT1")]
insample.campbel.bump<-data.campbel[,c("YEAR","PREDICT2")]
names(insample.campbel.trialheat)<-c("Year","Campbel.trialheat")
names(insample.campbel.bump)<-c("Year","Campbel.bump")


cuzan.data<-read.csv("~/Dropbox/EBMA/APSA_paper_2012/PresidentialForecasts/data_cuzan.csv")
summary(cuzan.data)



cuzan.long<-subset(cuzan.data,Year>1879)
cuzan.short<-subset(cuzan.data,Year>1915)

#results not exactly the same
cuzan1.short<-lm(VOTE2~FISCAL+GROWTH+NEWS+DUR+PARTY,data=cuzan.short)
cuzan1.long<-lm(VOTE2~FISCAL+GROWTH+NEWS+DUR+PARTY,data=cuzan.data)

cuzan2.long<-lm(VOTE2~FPRIME+GROWTH+NEWS+DUR+PARTY,data=cuzan.data)
cuzan2.short<-lm(VOTE2~FPRIME+GROWTH+NEWS+DUR+PARTY,data=cuzan.short)
years.long<-cuzan.long[,"Year"]
years.short<-cuzan.short[,"Year"]

insample.cuzan1.short<-fitted(cuzan1.short)
insample.cuzan2.short<-fitted(cuzan2.short)
insample.cuzan1.long<-fitted(cuzan1.long)
insample.cuzan2.long<-fitted(cuzan2.long)
insample.cuzan.short<-data.frame(years.short,insample.cuzan1.short,insample.cuzan2.short)
insample.cuzan.long<-data.frame(years.long,insample.cuzan1.long,insample.cuzan2.long)
names(insample.cuzan.short)<-c("Year","Cuzan1.short","Cuzan2.short")
names(insample.cuzan.long)<-c("Year","Cuzan1.long","Cuzan2.long")


hibbspreds <- read.csv("~/Dropbox/EBMA/APSA_paper_2012/PresidentialForecasts/Predictions_hibbs.csv")
insample.hibbs<-hibbspreds[,c("Year","Point_Pred")]
names(insample.hibbs)<-c("Year","Hibbs")

merge1<-merge(insample.campbel.trialheat,insample.campbel.bump,by="Year",all.x=TRUE,all.y=TRUE)
merge2<-merge(merge1,insample.holbrook,by="Year",all.x=TRUE,all.y=TRUE)
merge3<-merge(merge2,insample.cuzan.long,by="Year",all.x=TRUE,all.y=TRUE)
merge4<-merge(merge3,insample.cuzan.short,by="Year",all.x=TRUE,all.y=TRUE)

insample.data<-merge(merge4,insample.hibbs,by="Year",all.x=TRUE,all.y=TRUE)

