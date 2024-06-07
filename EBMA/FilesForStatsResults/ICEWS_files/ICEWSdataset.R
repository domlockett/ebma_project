library(ICEWS2)
data(icews)
data(Wmatrices)

# Duke files

dataICEWS<-data.frame(icews$insurgency,icews$pop.l3,icews$proximitytoelection,icews$parcomp,icews$xconst,icews$W.events.insurgency.l3,icews$gdppc.l3,icews$gdpgrowth.l3,as.character(icews$country),icews$year,icews$month, stringsAsFactors=FALSE) 
names(dataICEWS)<-c("insurgency","pop.l3","proximitytoelection","parcomp","xconst","W.events.insurgency.l3","gdppc.l3","gdpgrowth.l3","country","year","month")

summary(dataICEWS)

setwd("/Volumes/ICEWS Project/BMA Stuffs/ICEWS_files/")
write.csv(dataICEWS, file="dataICEWS.csv", row.names=FALSE)

