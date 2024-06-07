## Montgomery, Hollendbach, and Ward
## Replication files necessary for replicating
## Table 5
## Last edited: Jacob M. Montgomery
## Date: 12-19-2011

# You will want to replace this line with the appropriate directory
setwd("/Users/jmontgomery/Dropbox/EBMA/PRESIDENTIAL DATA for Jacob/Final submission")
library(foreign)
library(ensembleBMA)
library(EBMAforecast) # This package will be distribute on CRAN

# Read in data.  
courts <- read.dta("outcomelong.dta")

# Reshape so we have one row for each case, with all expert codings for all justices listed in the same row
# Dropping a bunch of variables while I'm at it
wide <- reshape(data=courts,
                 v.names=c("rehn", "stev", "scal", "kenn",
                   "thom", "gins", "brey", "ocon", "sout", "expert"),
                idvar="docket", direction="wide", timevar="number",
                drop=c("votecse","survey","frstsur","idctbel",
                  "dvctbel","disagct","idpet","idres","idcoun","qualbrf",
                  "preced","dicta","othstat","txtcon","txtstat","txtreg",
                  "nontext","intrpth","pracons","plcyprf","ideolgy","pubopin",
                  "congres","execut","profbck","persbck","noamici","idamici",
                  "sgamic","howconf","case1","case2","case3","case4","chair",
                  "category","datedeg","otherdeg","datebar","sctclerk","sctterm1",
                  "sctjust1","sctterm2","sctjust2","yrsteach","yrspract"))



# Making a basline dataset (organized by case) containing:
# docket, casename, certdate, arguedate, ctbelow, and variables used in the classification models (although we don't use thse
base <- wide[,c(1:8, 18:26, 36:40)]

# Making a dataset (organized by case) containing all information about what Rehnquist did, what the model predicted, what experts predicted, and expert ids
rehn <- wide[,c("rehnact", "rehnmod", "rehn.1", "rehn.2", "rehn.3", "expert.1", "expert.2", "expert.3")]
colnames(rehn) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3") # Renaming variables
rehn <- cbind(rehn, base) # combining this with the baseline dataset

# Repeat for Stevens
stev <- wide[,c("stevact", "stevmod", "stev.1", "stev.2", "stev.3", "expert.1", "expert.2", "expert.3")]
colnames(stev) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
stev <- cbind(stev, base)

scal <- wide[,c("scalact", "scalmod", "scal.1", "scal.2", "scal.3", "expert.1", "expert.2", "expert.3")]
colnames(scal) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
scal <- cbind(scal, base)

kenn <- wide[,c("kenact", "kenmod", "kenn.1", "kenn.2", "kenn.3", "expert.1", "expert.2", "expert.3")]
colnames(kenn) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
kenn <- cbind(kenn, base)

thom <- wide[,c("thomact", "thommod", "thom.1", "thom.2", "thom.3", "expert.1", "expert.2", "expert.3")]
colnames(thom) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
thom <- cbind(thom, base)

gins <- wide[,c("ginact", "ginmod", "gins.1", "gins.2", "gins.3", "expert.1", "expert.2", "expert.3")]
colnames(gins) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
gins <- cbind(gins, base)

brey <- wide[,c("bryact", "brymod", "brey.1", "brey.2", "brey.3", "expert.1", "expert.2", "expert.3")]
colnames(brey) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
brey <- cbind(brey, base)

ocon <- wide[,c("oconact", "oconmod", "ocon.1", "ocon.2", "ocon.3", "expert.1", "expert.2", "expert.3")]
colnames(ocon) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
ocon <- cbind(ocon, base)

sout <- wide[,c("soutact", "soutmod", "sout.1", "sout.2", "sout.3", "expert.1", "expert.2", "expert.3")]
colnames(sout) <- c("act","mod", "e1", "e2", "e3", "expert.1", "expert.2", "expert.3")
sout <- cbind(sout, base)

# Combining them all so we now have a "long" dataset of case-justice observations
final <- rbind(rehn, stev, scal, kenn, thom, gins, brey, ocon, sout)
final$just <- sort(rep(c(1:9), 68))

# The number of experts who predict yes over total number polled for that case
total <- num.exp <- rep(NA, 612)
for (i in 1:nrow(final)){
 total[i] <- sum(final$e1[i]+final$e2[i]+final$e3[i], na.rm=T)
 num.exp[i] <- sum(!is.na(c(final$e1[i], final$e2[i], final$e3[i])))
}
final$exp.tot <- total/num.exp

write.csv(final, "CourtsData.csv")

