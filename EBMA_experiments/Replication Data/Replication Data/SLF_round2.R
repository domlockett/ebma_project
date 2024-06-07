
logist<- function(x){
		ff<- 1/(1 + exp(-x))
		return(ff)
	}
	
len<- length
#setwd("~/Dropbox/creditClaimingProjects/het/")

#load('~/dropbox/creditClaimingProjects/het/Experiment1.RData')

##this file puts together the super learner for classifiers 
##to do this, we'll load a series of the functions with wrappers.
##then we'll run the super learning code in order to learn the variability

# fit various models
# This function will standardize the X matrix, fit a bunch of classifiers to
# The X matrix, then predict on the Xt (test) predictors. 

# It is built in such a way that one can use FindIt to compute a separate penalty
# matrix for covariates and treatment+het treatment effects

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
	fit1<- cv.glmnet(y = Y, x= Xfull, alpha=1, family='binomial', type='mse')
	#fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.75, family='binomial', type='mse')
	fit3<- cv.glmnet(y = Y, x= Xfull, alpha=0.5, family='binomial', type='mse')
	fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')
	
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
	fit6<- bayesglm(Y~Xfull-1, family=binomial(link=logit))
	cat("Finished bayesglm.. ")


	##next we use boosting
	if(speed==F){	
	#library(mboost)
	#library(GAMBoost)
	#fit7<- GLMBoost(Xfull[,-1],Y,penalty= 100,stepno=100,  trace = T,  family=binomial())
	#cat("Finished glmboost.. ")
	
	##now we use the bart method
	
	library(BayesTree)
	fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)
	cat("Finished bart.. ")
		
	
	print("Fitting randomForest.. ")
	library(randomForest)
	fit9<- randomForest(y = factor(Y), x = Xfull)
	}

	
	# Skip glm
#	fit10<- glm(Y~X, family=binomial(link=logit))
#	cat("Finished glm.. ")
	# I always think this says KRSOne
	
	if(speed==F){
	print("Fitting KRLS.. ")

	library(KRLS)
	fit11<- krls(X = Xfull[,-1], y = Y, derivative=F)
	}
	# this is probably not the most efficient way to run this model but whatever it
	# seems to consistently outperform e1071.
	# And finally SVM-SMO
	cat("Fitting SVM-SMO.. ")
	
	# Set java's max memory to 4 gigs:
	library(rJava)
	.jinit(parameters="-Xmx4g")
	library(RWeka)
	

	fit12 <- SMO(Y ~ ., data = data.frame(Y=factor(Y),Xfull),
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


supLearnFit <- function(X, treats, Y, nfold = 10, speed=T){
	
	# first straighten out data:
	if(class(X)=='matrix' | class(X)=='data.frame' & any(is.na(X)) ==F){
				toInclude <- complete.cases(cbind(X,treats,Y))	
				X <- X[toInclude,]}
	if(class(X)!= 'matrix' & class(X) != 'data.frame' & any(is.na(X))==F){		
			toInclude <- complete.cases(cbind(X,treats,Y))
				X<- X[toinclude]}
	if(any(is.na(X))==T){
			toInclude<- complete.cases(cbind(treats, Y))
			} 			
			
			
	Y <- Y[toInclude]
	if(class(treats)=='matrix'|class(treats)=='data.frame'){
	treats <- treats[toInclude,]
	}
	if(class(treats)!= 'matrix' & class(treats)!= 'data.frame'){
		treats<- treats[toInclude]}
	
	#Xfull <- model.matrix(~X*treats)
	Xfull<- X
	
	##this is the quadratic programming solution to the weights on the forecasters
	regress.func <- function(Y, preds.var){
		
		# need to smartly figure out which columns are not NA 
		orgcols <- length(preds.var[1,])
		notNA <- which(!is.na(preds.var[1,]))
		predX <- preds.var[,notNA ]
		
		library(quadprog)
		d.mat <- solve(chol(t(predX)%*%predX))
		a.mat <- cbind(rep(1, ncol(predX)), diag(ncol(predX)))
		b.vec <- c(1, rep(0, ncol(predX)))
		d.vec <- t(Y) %*% predX
		out<- solve.QP(Dmat = d.mat, factorized =TRUE, dvec = d.vec, Amat = a.mat, bvec = b.vec, meq = 1)
		coefs <- rep(NA, orgcols)
		notDel <- c(1:orgcols)[notNA]#[notCor]
		coefs[notDel] <- out$solution
		return(coefs)
	}
		
	## 10fcv 
	fold <- sample(nfold, nrow(X), replace = T)
	preds.var <- matrix(NA, nrow=nrow(X),ncol=12)
	
	wrapresults <- list()
	# This will store each CV pred fold
	for (z in sort(unique(fold))) {
		print(cat(paste('Fold', z, sep = ' ' ), '\n'))
		X.train<- Xfull[fold != z,]
		X.test <- Xfull[fold == z,]
		Y.train <- Y[fold != z]
		Y.test <- Y[fold == z]
		if(is.null(ncol(treats))==F){
		treats.train <- treats[fold != z,]
		treats.test <- treats[fold == z,]
		}
		if(is.null(ncol(treats))==T){
		treats.train <- treats[fold != z]
		treats.test <- treats[fold == z]}
		
		
#		debugonce(wrap.func)
		wrapresults[[z]] <- wrap.func(X.train, Y.train, 
				treats.train, X.test, Y.test, treats.test, speed=speed)[[1]]
		preds.var[fold == z,] <- wrapresults[[z]]
	}
		
	# Now run the function:
	store <- regress.func(Y, preds.var)
	store
	names(store) <- c('lasso', 'e_net_0.75', 'e_net_0.5', 'e_net_0.25', 'FindIt', 
			'BayesGLM', 'GLMBoost', 'BART', 'RandomForest', 'glm', 'KRLS', 'SVM-SMO')
	print(round(store, 4))
	return( list(store, preds.var) )
}

len<- length


predict.weight<- function(weights, models, new.treats, new.conts, X.train, Y){
	weights_use<- na.omit(weights[[1]])
	use<- 1:len(weights_use)
	preds.treat<-preds.cont<- matrix(NA, nrow = nrow(new.treats), ncol=len(use))
	a<- 0
	for(z in use){
		if(class(models[[2]][[z]])[1] =='cv.glmnet'){
			a<- a + 1
			preds.treat[,a]<-logist(predict(models[[2]][[z]]$glmnet.fit, newx = new.treats, s= models[[2]][[z]]$lambda.min))
			preds.cont[,a]<-logist(predict(models[[2]][[z]]$glmnet.fit, newx = new.conts, s= models[[2]][[z]]$lambda.min))

			print('glmnet')
			}
		
		if(class(models[[2]][[z]])[1]=='FindIt'){
			a <- a + 1
			preds.treat[,a]<- (new.treats%*%models[[2]][[z]]$coefs)/2 + 0.5
			preds.cont[,a]<- (new.conts%*%models[[2]][[z]]$coefs)/2 + 0.5
			print('FindIt')
			}
		if(class(models[[2]][[z]])[1]=='bayesglm'){
			a <- a + 1
			preds.treat[,a]<- logist(new.treats%*%models[[2]][[z]]$coef)
			preds.cont[,a]<- logist(new.conts%*%models[[2]][[z]]$coef)
			print('GLM')
			}
		if(class(models[[2]][[z]])[1]=='krls'){
			a<- a + 1
			##we need to do this by the 10k 
			if(nrow(new.treats)<10000){
				preds.treat[,a]<- predict(models[[2]][[z]], newdata = new.treats[,-1])$fit
				preds.cont[,a]<- predict(models[[2]][[z]], newdata= new.conts[,-1])$fit
			}
			if(nrow(new.treats)>10000){
				art<- nrow(new.treats)/10000
				rems<- nrow(new.treats)%%10000
				seqs<- seq(1, nrow(new.treats) - rems, by=10000)
				seqs<- c(seqs, nrow(new.treats) - rems, nrow(new.treats[,-1])) 
				for(aa in 1:(len(seqs)-1)){
					preds.treat[seqs[aa]:seqs[aa+1],a]<- predict(models[[2]][[z]], newdata = new.treats[seqs[aa]:seqs[aa+1], -1])$fit
					preds.cont[seqs[aa]:seqs[aa+1],a]<- predict(models[[2]][[z]], newdata= new.conts[seqs[aa]:seqs[aa+1], -1])$fit
			}
			}
				
				
			print('KRLS')
			}
		if(class(models[[2]][[z]])[1]=='SMO'){
			a<- a + 1
			preds.treat[,a]<- predict(models[[2]][[z]], newdata=data.frame(new.treats), type="probability" )[,2] 
			preds.cont[,a]<- predict(models[[2]][[z]], newdata = data.frame(new.conts), type="probability" )[,2] 
			print('SMO')
			}
		if(class(models[[2]][[z]])[1]=='glmboost'){
			a<- a + 1
			preds.treat[,a]<- 1 -  (attr(coef(models[[2]][[z]]), 'offset') + coef(models[[2]][[z]])%*%t(new.treats[,c(1,sort(unique(selected(models[[2]][[z]]))))]))
			preds.cont[,a]<- 1 - (attr(coef(models[[2]][[z]]), 'offset') + coef(models[[2]][[z]])%*%t(new.conts[,c(1,sort(unique(selected(models[[2]][[z]]))))]))
			print('GLMBoost')
			
	}
		if(class(models[[2]][[z]])[1]=='bart'){
			a<- a + 1
			
			pred.bart<- bart(x.train = X.train, y.train = Y, x.test = rbind(new.treats, new.conts), ndpost=1000, nskip=500, usequants=T)
			t.nums<- 1:nrow(new.treats)
			c.nums<- (nrow(new.treats) + 1):(nrow(new.treats)  + nrow(new.conts))
			preds.treat[,a]<- pnorm(apply(pred.bart$yhat.test[,t.nums], 2, mean))
			preds.cont[,a]<- pnorm(apply(pred.bart$yhat.test[,c.nums], 2, mean))
			
			print("BART")
			}
		if(class(models[[2]][[z]])[1]=='randomForest'){
			a<- a + 1
			preds.treat[,a]<- predict(models[[2]][[z]], newdata = new.treats, type='prob')[,2]
			preds.cont[,a]<- predict(models[[2]][[z]], newdata = new.conts, type='prob')[,2]
			print('randomForest')
			}
		}
		
		output<- list(preds.treat, preds.cont, weights_use)
		names(output)<- c('Treated', 'Control', 'Weights')
		return(output)
		}




