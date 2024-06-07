# ensemble BMA test run
# mdw, june 29, 2010
library(foreign)
library(ensembleBMA)
library(xtable)
library(reshape)

dd<-c("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/data")
od<-c("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/output")
setwd(dd)
dat<-read.csv(header=T,"/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/data/Ethnic Religious Violence.csv")

dat<-rename(dat, c(DukeGeospatial="duke",PSTraditional="psu",SAETraditional="sae",LCPSI="lustik",IDIEOIAggregator="idi",GTDS="gtds"))

dat1<-data.frame(dat[1:4351,] ) # remove cases with NA on gtds/need to be changed?
dat1<-dat1[-4351,]
library(BMA)

x<-dat[,c(3,5,6,7)]
y<-dat[,8]
bma.fit<- bic.glm(x, y, strict = FALSE, OR = 20, glm.family="binomial", 
    factor.type=TRUE)
    
c<-cor(dat[,c(3,4,5,6,7,8)],use="complete.obs") 
print(xtable(c))   
    
plot(density(bma.fit$postprob))    
summary(bma.fit)
imageplot.bma(bma.fit)
blub1<-summary(bma.fit)
tab2 <- xtable(blub1)
print(tab2)

b0<-bma.fit$postmean[1]
b1<-bma.fit$postmean[2]*bma.fit$x[,1]
b2<-bma.fit$postmean[3]*bma.fit$x[,2]
b3<-bma.fit$postmean[4]*bma.fit$x[,3]
eta<-b0 + b1 + b2 + b3
pred<-(exp(eta))/(1+ exp(eta))


pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/BMAdensityEth.pdf",
    width=5, height=5)
    
plot(density(pred,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="black",main="")



dev.off() 


