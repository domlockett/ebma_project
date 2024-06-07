library(EBMAforecast)
data(Insample)
data(Outsample)


in.data <- cbind(Insample[,"SAE"],  Insample[,"Lmer3"], Insample[,"Glm.simple"])
colnames(in.data) <- c("SAE", "LMER", "GLM")
out.data <- cbind(Outsample[,"SAE"], Outsample[,"Lmer3"], Outsample[,"Glm.simple"])

#in.data <- cbind(Insample[,"Polisci"],  Insample[,"Lmer.all"],  Insample[,"SAE"])
#colnames(in.data)<-c("Lmer Politics", "Lmer Politics 2", "SAE")
#out.data <- cbind(Outsample[,"Polisci"], Outsample[,"Lmer.all"], Outsample[,"SAE"])


y.in <- Insample$Insurgency

y.out <- Outsample$Insurgency

# Fit the BMA Model
BMA.fit <- Ensemble.logit(y=y.in, pp.raw=in.data, tol=.0001, exp=4)

# These are the within-sample model statistics/estimates for the parameters shown in the paper
print.Ensemble.logit(BMA.fit)
plot.Ensemble.logit(BMA.fit)


BMA.pred <- predict.Ensemble.logit(obj=BMA.fit, newdata=out.data, y.out=y.out)

# These are the out-of-sample model statistics (Model parameters are passed from the fit)
print.Ensemble.logit(BMA.pred)
plot.Ensemble.logit(BMA.pred)
