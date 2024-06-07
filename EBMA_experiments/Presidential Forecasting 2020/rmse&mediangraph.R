library(yardstick)
#How I got RMSE
 # dataRMSE <- as.data.frame(NA)
 # for(i in 2:18){
 #   dataRMSE <- cbind(dataRMSE,round(yardstick::rmse(data, data[,i],'truth', na.rm=T)[3],2))
 # }
 # colnames(dataRMSE)<-colnames(data)
 # data <- rbind(data,dataRMSE)
 # write.csv(data,'RMSE.csv')

data <- read.csv("C:/Users/dl0ck/Dropbox/EBMA_experiments/Presidential Forecasting 2020/Percent2PartyVote .csv")

prediction <- c(47.1, 58.1,52.8,53.8,52.7,51.3,50.7)
gallup <- c()
gdp <- c()
model <- lm(prediction ~ gallup + gdp)
prediction <- predict.lm(model,)
pdf("C:/Users/dl0ck/Dropbox/EBMA_experiments/Presidential Forecasting 2020/Graph2020.pdf", width=8, height=4)
data <- data[-8,]
colnames(data)[1] <- 'year'
data$medians <-NA
for(i in 1:7){
 data$medians[i] <- round(median(as.matrix(data[i,2:16]), na.rm=T),2)
 }


par(tcl=0, mgp=c(1,0,0), cex.lab=1, cex.axis=.9, mar=c(1,3,1,1))
plot(data$year, data$truth, ylim=c(42,60), pch=19, col="gray70", cex=2, xlim=c(1992, 2016), xaxt="n", xlab="", ylab="Incumbent Two-Pary Vote Share")
abline(h=50, lty=3, col="gray70")
mycol="gray50"
points(data$year, data$Abramowitz, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Campbell, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Cuzan, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Erikson.and.Wlezien, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Hibbs, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Holbrook, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Lewis.Beck.Tien, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Lockerbie, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$N.B, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Fair, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Jay.DeSart, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Andreas.Graefe, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Jerome.Bruno.Jerome.Veronique.and.Richard.Nadeau, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Allan.Lichtman, ylim=c(44,56),  type="l", col=mycol)
points(data$year, data$Thomas.Rietz.Joyce.Berg.Forrest.Nelson.Robert.Forsythe, ylim=c(44,56),  type="l", col=mycol)

#points(data$Year, data$means, type="l", col="darkblue", lwd=3)
points(data$year, data$medians, type="l", col="darkred", lwd=4, lty=1)
mtext(data$year, at=data$year, side=1, cex=.9)
dev.off()