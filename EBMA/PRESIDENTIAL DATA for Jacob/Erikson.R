library(foreign)
setwd("/Users/florianhollenbach/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/")
data<-read.dta("Erikson_Wlezien_Data.dta")
summary(data)


data1<-data[data$Year>=1952,]



################ Erikson_Wlezien column 2 in table 2 2008, i.e. second quarter models for all years 


### for 1976
data1976<-data1[data1$Year<=1972,]
data1976.2<-subset(data1976,Cycle==14)	
x76<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1976.2)
summary(x76)
var76<-subset(data1,Year==1976 & Cycle==14)
predict.lm(x76,var76)

### for 1980
data1980<-data1[data1$Year<=1976,]
data1980.2<-subset(data1980,Cycle==14)	
x80<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1980.2)
summary(x80)
var80<-subset(data1,Year==1980 & Cycle==14)
predict.lm(x80,var80)

### for 1984
data1984<-data1[data1$Year<=1980,]
data1984.2<-subset(data1984,Cycle==14)	
x84<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1984.2)
summary(x84)
var84<-subset(data1,Year==1984 & Cycle==14)
predict.lm(x84,var84)

### for 1988
data1988<-data1[data1$Year<=1984,]
data1988.2<-subset(data1988,Cycle==14)	
x88<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1988.2)
summary(x88)
var88<-subset(data1,Year==1988 & Cycle==14)
predict.lm(x88,var88)

### for 1992
data1992<-data1[data1$Year<=1988,]
data1992.2<-subset(data1992,Cycle==14)	
x92<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1992.2)
summary(x92)
var92<-subset(data1,Year==1992 & Cycle==14)
predict.lm(x92,var92)

### for 1996
data1996<-data1[data1$Year<=1992,]
data1996.2<-subset(data1996,Cycle==14)	
x96<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data1996.2)
summary(x96)
var96<-subset(data1,Year==1996 & Cycle==14)
predict.lm(x96,var96)

### for 2000
data2000<-data1[data1$Year<=1996,]
data2000.2<-subset(data2000,Cycle==14)	
x00<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data2000.2)
summary(x00)
var00<-subset(data1,Year==2000 & Cycle==14)
predict.lm(x00,var00)

### for 2004
data2004<-data1[data1$Year<=2000,]
data2004.2<-subset(data2004,Cycle==14)	
x04<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data2004.2)
summary(x04)
var04<-subset(data1,Year==2004 & Cycle==14)
predict.lm(x04,var04)

### for 2008
data2008<-data1[data1$Year<=2004,]
data2008.2<-subset(data2008,Cycle==14)	
x08<-lm(IncumbentVote~ l1CumLEIGrowth + IncumbentPoll,data=data2008.2)
summary(x08)
var08<-subset(data1,Year==2008 & Cycle==14)
predict.lm(x08,var08)

############################# Fair model
