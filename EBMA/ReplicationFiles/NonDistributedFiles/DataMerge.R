# This file takes all of the raw data files to make the master.data
# Comments include our code to replicate the original analyses
# Written by Jacob M. Montgomery
# Last Edited 12-19-2011

setwd("/Users/jmontgomery/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/Final submission")
library(foreign)

### Make a master dataset with everything needed for all analyses

   master.data <- data.frame(matrix(NA, 24, 1))
   colnames(master.data) <- "dv"
   rownames(master.data) <- seq(1916,2008, by=4)
   master.data$year <- seq(1916,2008, by=4)

# Add in  campbell data
   campbell <- read.csv("Campbell_Data.csv")
   master.data$dv[9:24] <- campbell$INPTYVOTE
   master.data$septpoll[9:24] <- campbell$SEPTPOLL
   master.data$gdpqtr2half[9:24] <- campbell$GDPQTR2HALF

## Add in lewis-beck data
   lb <- read.csv("Lewis-Beck_Tien_Data.csv")
   lb <- lb[1:16, 1:7]
   master.data$julypop[9:24] <- lb$JULYPOP
   master.data$incxgnp[9:24] <- lb$INCXGNP
   master.data$jobhousu[9:24] <- lb$JOBHOUSU
   master.data$closeinc[9:24] <- lb$CLOSEINC

# Add in Erickson-Wlezien Data

   ew <- read.dta("Erikson_Wlezien_Data.dta")
   ew$IncumbentPoll[13] <- ew$IncumbentPoll[14]
   ew.red <- ew[ew$Year<=2008 & ew$Cycle==14,]
   ew.red <- ew.red[1:15,]
   master.data$l1cumleigrowth[10:24] <- ew.red$l1CumLEIGrowth
   master.data$incumbentpoll[10:24] <- ew.red$IncumbentPoll


#### Fair Data
    fair <- read.csv("Fair_Data.csv")
    fair <- fair[fair$Year>=1916 & fair$Year<=2008,]
    master.data$G <- fair$G
    master.data$P <- fair$P
    master.data$Z <- fair$Z
    master.data$adper <- abs(fair$DPER)
    master.data$adur <-  abs(fair$DUR)
    master.data$war <-  fair$WAR
    master.data$I <-  fair$I
    temp.dv <- ((fair$VP*fair$I) + 50*(1-fair$I))
    master.data$dv[1:8] <- temp.dv[1:8] #For early years, we are using Fair's estimates of the two-party vote share

### Add Hibbs data

     hibbs <- read.dta("Hibbs.dta")
     hibbs$r <- hibbs$dpi_pc/(hibbs$cpi_sa_8284/100)
     hibbs$lnr <- log(hibbs$r)
     hibbs$dnlr <- NA
     for (i in 2:nrow(hibbs)){
       hibbs$dnlr[i] <- (hibbs$lnr[i]-hibbs$lnr[i-1])*400
     }

     for(i in 16:nrow(hibbs)){ # Making lagged terms
       hibbs$dnlr.L1[i] <- hibbs$dnlr[i-1]
       hibbs$dnlr.L2[i] <- hibbs$dnlr[i-2]
       hibbs$dnlr.L3[i] <- hibbs$dnlr[i-3]
       hibbs$dnlr.L4[i] <- hibbs$dnlr[i-4]
       hibbs$dnlr.L5[i] <- hibbs$dnlr[i-5]
       hibbs$dnlr.L6[i] <- hibbs$dnlr[i-6]
       hibbs$dnlr.L7[i] <- hibbs$dnlr[i-7]
       hibbs$dnlr.L8[i] <- hibbs$dnlr[i-8]
       hibbs$dnlr.L9[i] <- hibbs$dnlr[i-9]
       hibbs$dnlr.L10[i] <- hibbs$dnlr[i-10]
       hibbs$dnlr.L11[i]<- hibbs$dnlr[i-11]
       hibbs$dnlr.L12[i] <- hibbs$dnlr[i-12]
       hibbs$dnlr.L13[i] <- hibbs$dnlr[i-13]
       hibbs$dnlr.L14[i] <- hibbs$dnlr[i-14]
     }

    hibbs.real<- hibbs[!is.na(hibbs$presvote),]

    master.data$fatalities[10:24] <- hibbs.real$Fatalities
    master.data$wtq16[10:24] <- hibbs.real$wtq16
    master.data$dnlr[10:24]  <- hibbs.real$dnlr
    master.data$dnlr.L1[10:24] <- hibbs.real$dnlr.L1
    master.data$dnlr.L2[10:24] <- hibbs.real$dnlr.L2
    master.data$dnlr.L3[10:24] <- hibbs.real$dnlr.L3
    master.data$dnlr.L4[10:24] <- hibbs.real$dnlr.L4
    master.data$dnlr.L5[10:24] <- hibbs.real$dnlr.L5
    master.data$dnlr.L6[10:24] <- hibbs.real$dnlr.L6
    master.data$dnlr.L7[10:24] <- hibbs.real$dnlr.L7
    master.data$dnlr.L8[10:24] <- hibbs.real$dnlr.L8
    master.data$dnlr.L9[10:24] <- hibbs.real$dnlr.L9
    master.data$dnlr.L10[10:24] <- hibbs.real$dnlr.L10
    master.data$dnlr.L11[10:24] <- hibbs.real$dnlr.L11
    master.data$dnlr.L12[10:24] <- hibbs.real$dnlr.L12
    master.data$dnlr.L13[10:24] <- hibbs.real$dnlr.L13
    master.data$dnlr.L14[10:24] <- hibbs.real$dnlr.L14


    ### Add Abrmowitz 
    ab <- read.csv("Abramowitz_data.csv")

    master.data$q2gdp[9:24] <- ab$q2gdp
    master.data$term[9:24] <- ab$term
    master.data$juneapp[9:24] <- ab$juneapp


    ### Write the data to a file
    write.csv(master.data, "presdata.csv")
