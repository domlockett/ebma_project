
dataFull<-read.csv("~/Dropbox/EBMA/ForecastData2016/PresidentforUse.csv")
data<-dataFull[1:6,]
colnames(data)
include.these<-c("Abramowitz","Campbell", "EW","LB.T","Lockerbie","N.B","Fair", "Holbrook")
data$means <- rowMeans(as.matrix(data[,include.these]), na.rm=TRUE)
data$medians <-    apply(as.matrix(data[,include.these]), 1, median, na.rm=TRUE)
data$medians


pdf("~/Dropbox/SFPresentation/Presentation/PotentialGraph.pdf", width=8, height=4)
par(tcl=0, mgp=c(1,0,0), cex.lab=1, cex.axis=.9, mar=c(1,3,1,1))
plot(data$Year, data$truth, ylim=c(42,60), pch=19, col="gray70", cex=2, xlim=c(1992, 2012), xaxt="n", xlab="", ylab="% of two-party vote going to \nthe incumbent party candidate")
abline(h=50, lty=3, col="gray70")
mycol="gray50"
points(data$Year, data$Abramowitz, ylim=c(44,56),  type="l", col=mycol)
points(data$Year, data$EW, ylim=c(44,56),  type="l", col=mycol)
points(data$Year, data$LB.T, ylim=c(44,56),  type="l", col=mycol)
points(data$Year, data$Lockerbie, ylim=c(44,56),  type="l", col=mycol)
points(data$Year, data$N.B, ylim=c(44,56),  type="l", col=mycol)
points(data$Year, data$Fair, ylim=c(44,56),  type="l", col=mycol)
#points(data$Year, data$means, type="l", col="darkblue", lwd=3)
points(data$Year, data$medians, type="l", col="darkred", lwd=4, lty=1)
mtext(data$Year, at=data$Year, side=1, cex=.9)
dev.off()


data$medians

colMeans(abs(data[,include.these]-data$truth), na.rm=TRUE)

sum(c(0.3709824, 0.06419703, 0.1428431, 0.02407423, 0.03144253, 0.05571408))
