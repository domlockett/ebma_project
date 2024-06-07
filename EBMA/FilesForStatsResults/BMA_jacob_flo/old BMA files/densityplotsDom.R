pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDom.pdf",
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

pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomB.pdf",
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

#Duke vs. truth
pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomDuke.pdf",
    width=5, height=5)
    
plot(density(dat$duke,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="blue",ylim=c(0,35),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("blue","black")

legend(.6, 25, c("Duke","Truth"),
       text.col=mycol,bty="n")
dev.off() 

#PSU vs. truth
pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomPSU.pdf",
    width=5, height=5)
    
plot(density(dat$psu,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="red",ylim=c(0,30),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("red","black")

legend(.6, 20, c("PSU","Truth"),
       text.col=mycol,bty="n")
dev.off() 

#SAE vs. truth
pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomSAE.pdf",
    width=5, height=5)
    
plot(density(dat$sae,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="purple",ylim=c(0,35),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("purple","black")

legend(.6, 25, c("SAE","Truth"),
       text.col=mycol,bty="n")
dev.off() 

#Lustik vs. Truth
pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomLustik.pdf",
    width=5, height=5)
    
plot(density(dat$lustik,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="green",ylim=c(0,15),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("green","black")

legend(.6, 10, c("Lustik","Truth"),
       text.col=mycol,bty="n")
dev.off() 

    
#IDI vs. Truth
pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomIdi.pdf",
    width=5, height=5)
    
plot(density(dat$idi,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="orange",ylim=c(0,100),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("orange","black")

legend(.6,75, c("IDI","Truth"),
       text.col=mycol,bty="n")
dev.off() 

#pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityDomC.pdf",
 #   width=5, height=5)
    
#plot(density(dat$lustik,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="green",ylim=c(0,5),main="")


#lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
#mycol<-c("green","black")

#legend(.6, 4, c("Lustik","Truth"),
 #      text.col=mycol,bty="n")
#dev.off() 