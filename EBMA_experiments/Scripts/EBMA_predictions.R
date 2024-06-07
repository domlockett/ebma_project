library(glmnet)
library(FindIt)
library(arm)
library(mboost)
library(GAMBoost)
library(BayesTree)
library(randomForest)
library(KRLS)
library(rJava)
.jinit(parameters="-Xmx4g")
library(RWeka)
setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments")
jan <- read.csv('January Wave/Data_0219/jan_HL.csv')
oct <- read.csv('October Wave/Data_1018/oct_HL.csv')

dat <- as.matrix(jan[, c("HL_accuracy","treat", "totalfakenewscount_new","pol_therm_media","repub_leaners","independents","polknow","fake", "female", "college", "agecat", "dem_leaners","polint", "ideology", "conspiracy_mean", "proD", "proR", "highProm","lowProm","hyper","real", "weight")])

#many classifiers don't run with NA
dat <- na.omit(dat)

#create test and training index
train_index <- sample(1:nrow(dat), 0.8 * nrow(dat))
test_index <- setdiff(1:nrow(dat), train_index)

# The paper only has 7 models: It excludes random forest and BART. 
# Their code originally had 12, but they commented out 3 of the models: GLM at .75, GLM, and GLMBoost
# Leaving the 9 models I have in this script
#currently I cannot get FindIt and KRLS to run

# Build X_train, y_train, X_test, y_test
X <- dat[train_index, -c(1,2)]
Y <- dat[train_index, "HL_accuracy"]
treat <- dat[train_index, "treat"]


Xt <- dat[test_index, -c(1,2)]
Yt <- dat[test_index, "HL_accuracy"]
treatt <- dat[test_index, "treat"]


# First do standardization for FindIt is happy:
# first standardize data:
mkstand <- function(x){
  x <- x - mean(x, na.rm=T)
  if(sd(x, na.rm=T) >0){
    x <- x/sd(x, na.rm=T)
  }
  return(x)
}


# Now compute standard deviations to rescale data:
## Dom: IDK if this is neccessary, running based off of Grimmer et al script:

if(is.null(X)==F){
  SDsToRescaleX <- apply(X, 2, sd, na.rm=T)
  
  # if sd=0, make rescaling factor 1 (so it's not NaN)
  SDsToRescaleX[SDsToRescaleX==0] <- 1 
  SDsToRescaleXt <- apply(Xt, 2, sd, na.rm=T) 
  SDsToRescaleXt[SDsToRescaleXt==0] <- 1 
  
  # standardize coeffs store result in new matrix
  Xstd <- apply(X, 2, mkstand)
  Xtstd <- apply(Xt, 2, mkstand)
  
  # Need to make ints for inclusion in original X matrix that is passed in
  # (This will be used for every model besides FindIt)
  Xfull <- model.matrix(~X*treat)
  Xtfull <- model.matrix(~Xt*treatt)	
}
if(is.null(X)==T){
  Xfull<- model.matrix(~treat)
  Xtfull<- model.matrix(~treatt)
}



# Set colnames consistently so that RandomForrest package is happy
colnames(Xfull) <- gsub("X", "", colnames(Xfull))
colnames(Xtfull) <- gsub("Xt", "", colnames(Xtfull))
colnames(Xtfull) <- gsub("treatt", "treat", colnames(Xtfull))


## the first methods are based on cv.glmnet
# varying alpha to observe lasso (a=1), elastic net (a=.5), and ridge (alpha =0) 
# May need to vary alpha to find best value for elastic net
fit1<- cv.glmnet(y =Y, x= Xfull, alpha=1)
fit2<- cv.glmnet(y =Y, x= Xfull, alpha=0.5)
fit3<- cv.glmnet(y =Y, x= Xfull, alpha=0)


##the next function is bayesglm from Gelman's program
fit4<-  bayesglm(Y~Xfull-1)


##now we use the bart method
# ndpost	
# The number of posterior draws after burn in, ndpost/keepevery will actually be returned.
# 
# nskip	
# Number of MCMC iterations to be treated as burn in.

fit5<- bart(x.train=Xfull, y.train=(Y[as.numeric(rownames(Xfull))]), x.test=Xtfull, ndpost=1000, nskip=500)

#Now do random forest

fit6<- randomForest(y = factor(Y[as.numeric(rownames(Xfull))]), x = Xfull)


#SVM-SMO
## Sequential minimal optimization algorithm for training a support
## vector classifier, using am RBF kernel with a non-default gamma
## parameter (argument '-G') instead of the default polynomial kernel
#Currently returning all 1's

# fit7 <- SMO(Y ~ ., data = data.frame(Y=factor(Y[as.numeric(rownames(Xfull))]),Xfull) ) 


# Now predict
pred.vals = NA
pred.vals<- matrix(NA, nrow=nrow(Xt), ncol=6)
# logist<- function(x){
#   ff<- 1/(1 + exp(-x))
#   return(ff)
# }

pred.vals[,1]<- (predict(fit1$glmnet.fit, newx = Xtfull, s = fit1$lambda.min))
pred.vals[,2]<- (predict(fit2$glmnet.fit, newx = Xtfull, s = fit3$lambda.min))

#Predicts 100 versions of lambda so I just average them.
pred.vals[,3] <- apply(predict(fit3$glmnet.fit, newx = Xtfull, s = fit4$lambda.min),1,mean)

##bayesglm
pred.vals[,4]<- (Xtfull%*%fit4$coef)

##Bart
pred.vals[,5]<- fit5$yhat.test.mean

#Random Forest
pred.vals[,6]<- predict(fit6, newdata=Xtfull)

#SVM-SMO
#pred.vals[,7]<- predict(fit7, newdata= data.frame(Xtfull) ) 

#model performance
class.perform <- matrix(cbind(NA,c('cv.glmnetLASSO','cv.glmnetELASTIC','cv.glmnetRIDGE','BayesGLM','BART','RF')), nrow=6, ncol =2)

for(i in 1:6){
  class.perform[i,1] <-sum(round(pred.vals[,i])==Yt)/length(Yt)
}
class.perform
#Confusion matrices

for(i in 1:6){
  print(c('cv.glmnetLASSO','cv.glmnetELASTIC','cv.glmnetRIDGE','BayesGLM','BART','RF')[i])
  print(table(factor(round(pred.vals[,i]), levels=min(Yt):max(Yt)), 
        factor(Yt, levels=min(Yt):max(Yt))))
}

#Models TROUBLESHOOTING

##FindIt

# This is how FindIt is supposed to run, with two-way interactions
# between all relevant factors in X.  
#	Xtw <- maketwoway(X)
#	ncol(X)
#	ncol(Xtw$X)
#	fit5 <- FindIt(Y,X.c=Xtw$X,treat, 
#			scale.c=Xtw$scale.X, 
#			search.lambdas=TRUE, 
#			fit.glmnet=TRUE) #Run to find the LASSO parameters

# instead, we will only use the X matrix + all treatment ints.
# First, we transform Y into -1,1 from 0,1
FIY <- Y
FIY[FIY==0] <- -1

if(is.null(ncol(treat))==F){
  colnames(Xstd)<- paste('Cov', 1:ncol(Xstd), sep='')
  colnames(treat)<- as.character(1:ncol(treat))
  start<- model.matrix(~Xstd*treat)
  treat2<- start[,which(colnames(start)=='treat1'):ncol(start)]
  fit8 <- FindIt(FIY,X.c=Xstd, treat2, type='multiple',
                 scale.c= SDsToRescaleX, 
                 search.lambdas=TRUE, 
                 fit.glmnet=TRUE,wts=1) }
if(is.null(ncol(treat)) == T){
  colnames(Xstd)<- paste('Cov', 1:ncol(Xstd), sep='')
  fit8 <- FindIt(FIY,X.c=Xstd, treat, type='single',
                 scale.c= SDsToRescaleX, 
                 search.lambdas=TRUE, 
                 fit.glmnet=TRUE, wts=1)	}


#KRLS

fit9<- krls(X = Xfull[,-1], y = Y, derivative=F)

## Once I figure them out, predict

#FindIt prediction
pred.vals[,8] <- (model.matrix(~Xtstd*treatt) %*% fit8$coefs)/2 + .5  

#KRLS prediction
pred.vals[,9]<- predict(fit9, newdata= Xtfull[,-1])$fit

fitstxt <- grep("fit", ls(), value=T)
reords<- paste('fit', 1:12, sep='')
fitstxt<- reords[which(reords %in% fitstxt)]


fits <- list()
for( i in 1:length(fitstxt)){
  fits[[i]] <- eval(parse(text=fitstxt[i]))
}
return(list(pred.vals, fits))
sum(Yt == pred.vals, na.rm=T)
pred.vals
