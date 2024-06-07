library(ICEWS2)

source("Ensemble.logit.R")
source("Ensemble.logit.pred.R")



data(icews)

in.sample <- icews[icews$year<2009,]
out.sample <- icews[icews$year==2009,]

model.polisci <-lmer(insurgency ~ xconst + parreg + nminorities + nminorities.l3 + (1|country), data = in.sample, family="binomial")
summary(model.polisci)
in.m1 <- fitted(model.polisci, newdata=icews )
out.m1 <- predict.lmer(model.polisci, newdata=out.sample)

glm.model.econ <-glm(insurgency ~ insurgency.l3 + gdppc.l3 + gdpgrowth, data = in.sample, family="binomial")
summary(glm.model.econ)
in.m2 <- fitted(glm.model.econ)
out.m2 <- predict(glm.model.econ, type="response", newdata=out.sample)

model.lmer.econ<-lmer(insurgency ~ gdpgrowth + nminorities + (1 | country), data=in.sample, family="binomial")
summary(model.lmer.econ)
in.m3 <- fitted(model.lmer.econ)
out.m3 <- predict.lmer(model.lmer.econ, newdata=out.sample)

model.glm.simple<-glm(insurgency~gdpgrowth + nminorities, data=in.sample, family="binomial")
summary(model.glm.simple)
in.m4 <- fitted(model.glm.simple)
out.m4 <- predict(model.glm.simple, type="response", newdata=out.sample)

model.lmer.alltheway<-lmer(insurgency~insurgency.l3 + parcomp + antiGovtviolence.l3 + gdpgrowth + (1 + gdppc.l3 | country ) , data=in.sample, family="binomial")
summary(model.lmer.alltheway)
in.m5 <- fitted(model.lmer.alltheway)
out.m5 <- predict.lmer(model.lmer.alltheway, newdata=out.sample)

in.data <- cbind(in.m1, in.m2, in.m3, in.m4, in.m5)
out.data <- cbind(out.m1, out.m2, out.m3, out.m4, out.m5)
y.in <- in.sample$insurgency
y.out <- out.sample$insurgency

BMA.fit <- Ensemble.logit(y=y.in, pp.raw=in.data)
BMA.pred <- predict.Ensemble.logit(obj=BMA.fit, newdata=out.data, y.out=y.out,make.plot=TRUE)



