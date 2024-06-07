library(glmnet)
library(psycho)


setwd("C:/Users/dl0ck/Dropbox/EBMA_experiments")
jan <- read.csv('January Wave/Data_0219/jan_HL.csv')
oct <- read.csv('October Wave/Data_1018/oct_HL.csv')

dat <- as.matrix(jan[, c("HL_accuracy","treat", "totalfakenewscount_new", "female", "college", "agecat", "dem_leaners","polint", "ideology", "conspiracy_mean", "proD", "proR", "highProm","lowProm","hyper","real", "weight")])

train_index <- sample(1:nrow(dat), 0.8 * nrow(dat))
test_index <- setdiff(1:nrow(dat), train_index)

# Build X_train, y_train, X_test, y_test
X <- dat[train_index, -c(1,2)]
Y <- dat[train_index, "HL_accuracy"]
treat <- dat[train_index, "treat"]


Xt <- dat[test_index, -c(1,2)]
Yt <- dat[test_index, "HL_accuracy"]
treatt <- dat[test_index, "treat"]

wrap.func <- function(X, Y, treat, Xt, Yt, treatt, speed=T){
  
  # this bit is for debugging:
  # X=X.train; Y=Y.train; 
  # treat=treats.train; Xt=X.test; Yt=Y.test; treatt= treats.test
  
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
  print("Fitting GLMNET.. ")
  library(glmnet)
  fit1<- cv.glmnet(y = Y[as.numeric(rownames(Xfull))], x= Xfull, alpha=1, family='binomial', typ='mse')
  #fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.75, family='binomial', type='mse')
  fit3<- cv.glmnet(y = Y[as.numeric(rownames(Xfull))], x= Xfull, alpha=0.5, family='binomial', type='mse')
  fit4<- cv.glmnet(y = Y[as.numeric(rownames(Xfull))], x= Xfull, alpha=0, family='binomial', type='mse')
  
  # Next FindIt
  #	install.packages("FindIt")
  if(speed==F){
    print("Fitting FindIt.. ")
    library(FindIt)
    
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
      fit5 <- FindIt(FIY,X.c=Xstd, treat2, type='multiple',
                     scale.c= SDsToRescaleX, 
                     search.lambdas=TRUE, 
                     fit.glmnet=TRUE,wts=1) }
    if(is.null(ncol(treat)) == T){
      colnames(Xstd)<- paste('Cov', 1:ncol(Xstd), sep='')
      fit5 <- FindIt(FIY,X.c=Xstd, treat, type='single',
                     scale.c= SDsToRescaleX, 
                     search.lambdas=TRUE, 
                     fit.glmnet=TRUE, wts=1)	}
  }
  # This crap is too slow, but could be added back in if we don't care about time.
  ##the next function is bayesglm from Gelman's program
  
  library(arm)
  fit6<- bayesglm(Y[as.numeric(rownames(Xfull))]~Xfull-1, family=binomial(link=logit))
  cat("Finished bayesglm.. ")
  
  
  ##next we use boosting
  if(speed==F){	
    #library(mboost)
    #library(GAMBoost)
    #fit7<- GLMBoost(Xfull[,-1],Y,penalty= 100,stepno=100,  trace = T,  family=binomial())
    #cat("Finished glmboost.. ")
    
    ##now we use the bart method
    
    library(BayesTree)
    fit8<- bart(x.train=Xfull, y.train=factor(Y[as.numeric(rownames(Xfull))]), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)
    cat("Finished bart.. ")
    
    
    print("Fitting randomForest.. ")
    library(randomForest)
    fit9<- randomForest(y = factor(Y[as.numeric(rownames(Xfull))]), x = Xfull)
  }
  
  
  # Skip glm
  #	fit10<- glm(Y~X, family=binomial(link=logit))
  #	cat("Finished glm.. ")
  # I always think this says KRSOne
  
  if(speed==F){
    print("Fitting KRLS.. ")
    
    library(KRLS)
    fit11<- krls(X = Xfull[,-1], y = Y[as.numeric(rownames(Xfull))], derivative=F)
  }
  # this is probably not the most efficient way to run this model but whatever it
  # seems to consistently outperform e1071.
  # And finally SVM-SMO
  cat("Fitting SVM-SMO.. ")
  
  # Set java's max memory to 4 gigs:
  library(rJava)
  .jinit(parameters="-Xmx4g")
  library(RWeka)
  
  
  fit12 <- SMO(Y ~ ., data = data.frame(Y=factor(Y[as.numeric(rownames(Xfull))]),Xfull),
               control = Weka_control(M = TRUE ) )
  
  ##Now predict them
  logist<- function(x){
    ff<- 1/(1 + exp(-x))
    return(ff)
  }
  
  # Now predict
  print("\nPredicting GLMNET")
  pred.vals<- matrix(NA, nrow=nrow(Xt), ncol=12)
  pred.vals[,1]<- logist(predict(fit1$glmnet.fit, newx = Xtfull, s = fit1$lambda.min))
  pred.vals[,3]<- logist(predict(fit3$glmnet.fit, newx = Xtfull, s = fit3$lambda.min))
  pred.vals[,4]<- logist(predict(fit4$glmnet.fit, newx = Xtfull, s = fit4$lambda.min))
  
  print("\nPredicting FindIt")
  # using maketwoway() doesn't work (chooses different from test vars to include 
  # so just use Xtstd.
  #	Xttw <- maketwoway(Xt)
  #	XtestFindit <- cbind(Xttw$X, Xttw$X*treatt)
  
  # Needs to be re-scaled to 0,1
  if(speed==F){
    pred.vals[,5] <- (model.matrix(~Xtstd*treatt) %*% fit5$coefs)/2 + .5  
  }
  
  pred.vals[,6]<- logist(Xtfull%*%fit6$coef)
  if(speed == F){	
    #pred.vals[,7]<-  (attr(coef(fit7), 'offset') + coef(fit7)%*%t(Xtfull[,c(1,sort(unique(selected(fit7))))]))
    
    
    
    pred.vals[,8]<- pnorm(apply(fit8$yhat.test, 2, mean))
    
    cat("\nPredicting randomForest")
    
    pred.vals[,9]<- predict(fit9, newdata=Xtfull, type='prob')[,2]
  }
  if(speed==F){
    print("\nPredicting KRLS")
    pred.vals[,11]<- predict(fit11, newdata= Xtfull[,-1])$fit
  }
  
  
  print("\nPredicting SVM-SMO")
  pred.vals[,12]<- predict(fit12, newdata= data.frame(Xtfull), type="probability" )[,2] 
  
  print("\nSaving Models")
  fitstxt <- grep("fit", ls(), value=T)
  reords<- paste('fit', 1:12, sep='')
  fitstxt<- reords[which(reords %in% fitstxt)]
  
  
  # Create a list with each model as the ith element so that we can return it 
  # easily as output
  fits <- list()
  for( i in 1:length(fitstxt)){
    fits[[i]] <- eval(parse(text=fitstxt[i]))
  }
  return(list(pred.vals, fits))
}

wrap.func(X,Y,treat,Xt,Yt,treatt,speed=T)

#ELASTIC NET: ALPHA .5



#ELASTIC NET: ALPHA.25



#FINDIT



#BAYESIAN GLM



#KRLS



#SVM-SMO


