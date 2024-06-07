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

pdf("/Users/florianhollenbach/Documents/Duke/Summer10/Ward/Task/ensembleBMA4Florian/graphics/5densityc.pdf",
    width=5, height=5)
    
plot(density(dat$lustik,na.rm=T),xlab="predicted probability",las=1,ylab="",bty="n",lwd=2,col="green",ylim=c(0,5),main="")


lines(density(dat$gtds,na.rm=T),lwd=2,col="black")
mycol<-c("green","black")

legend(.6, 4, c("Lustik","Truth"),
       text.col=mycol,bty="n")
dev.off() 