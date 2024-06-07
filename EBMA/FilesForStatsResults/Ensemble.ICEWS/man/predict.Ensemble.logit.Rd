\name{predict.Ensemble.logit}
\alias{predict.Ensemble.logit}
%- Also NEED an '\alias' for EACH other topic documented here.  
\title{
Generate Out-of-Sample Predictions Using Ensemble BMA Models for Binary Outcomes  
}
\description{Calculates out-of-sample forecasts on the predicted probability scale for the ensemble model.  Inputs include an \code{Ensemble.logit} object and out-of-sample predictions (on the predicted probability scale) generated from component forecasts.
}
\usage{
predict.Ensemble.logit(obj, newdata, y.out, make.plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An object created by the \code{\link{Ensemble.logit}} function
}
  \item{newdata}{
A matrix of out-of-sample predicted probabilities.  These should be generated from the same component models used to fit the \code{obj}
}
  \item{y.out}{
Observed outcomes for the out-of-sample dataset
}
  \item{make.plot}{
A switch indicating whether a separation plot should be created
}
}
\details{
This utility function is to be applied after users have fit an \code{\link{Ensemble.logit}} model.  Using new predictions from each of the component forecasts, it generates ensemble forecasts.  \cr
\cr
In addition, the function calculates fit statistics to compare the out-of-sample predictions from component models and the ensemble forecast. Setting \code{make.plot=TRUE} creates a separation plot for each component and the EBMA model. \cr
}
\value{
 \item{bma.pred}{The ensemble predictions (on the predicted probability scale) for the out-of-sample observations}
 \item{bma.auc}{Area under the ROC curve for the full BMA model}
 \item{models.auc}{Area under the ROC curve for each of the component forecasts}
 \item{bma.brier}{Brier statistic for the BMA model}
 \item{models.brier}{Brier statistics for each of the component forecasts}
}
\references{
Ward, M. D., J. M. Montgomery and F. M. Hollenbach. 2011 Improving Conflict Predictions Using Ensemble Bayesian Model Averaging.  Paper presented at the 2011 meeting of the International Studies Association in Montreal, Quebec.\cr
\cr
Raftery, A. E., T. Gneiting, F. Balabdaoui and M. Polakowski. 2005 Using Bayesian Model Averaging to calibrate forecast ensembles, \emph{Monthly Weather Review} \bold{133}:1155--1174.\cr
\cr
Sloughter, J. M., A. E. Raftery, T. Gneiting and C. Fraley. 2007 Probabilistic quantitative precipitation forecasting using Bayesian model averaging, \emph{Monthly Weather Review} \bold{135}:3209--3220.\cr
\cr
Fraley, C., A. E. Raftery, T. Gneiting, 2010 Calibrating Multi-Model Forecast Ensembles with Exchangeable and Missing Members using Bayesian Model Averaging, \emph{Monthly Weather Review} \bold{138}:190--202.\cr
\cr
Sloughter, J. M., T. Gneiting and A. E. Raftery, 2010 Probabilistic wind speed forecasting using ensembles and Bayesian model averaging, \emph{Journal of the American Statistical Association}, \bold{105}:25--35.
}
\author{
Jacob Montgomery <jacob.montgomery@duke.edu>
}
\note{
PLEASE NOTE: This function does not currently handle missing data, but does not provide errors.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Ensemble.ICEWS}}, \code{\link{Ensemble.logit}}, \code{\link{Insample}}, \code{\link{Outsample}}
}
\examples{
demo(Ensemble.ICEWS)

## The function is currently defined as
function(obj, newdata, y.out, make.plot=FALSE) {
  num.models <- ncol(obj$model.params)
  num.obs <- nrow(newdata)
  PP.matrix <- matrix(NA, nrow=num.obs, ncol=num.models)
  for(k in 1:num.models){PP.matrix[,k] <- 
  	   plogis(obj$model.params[1,k]+obj$model.params[2,k]*(newdata[,k]^(1/obj$exp)))}
  bma.pred <- as.vector(PP.matrix\%*\%obj$W)

# Calculate Area under ROC Curves
  bma.auc <- somers2(bma.pred,y.out)[1]
  models.auc <- rep(NA, num.models)
  for(i in 1:num.models){models.auc[i] <- somers2(newdata[,i],y.out)[1]}

# Calculate Brier (sp?) statistic
  bma.brier <- mean((y.out-bma.pred)^2)
  models.brier <- rep(NA, num.models)
  for(i in 1:num.models){models.brier[i] <-mean((y.out-newdata[,i])^2)}

# Do Plot
  if (make.plot){
    par(mgp=c(3, 0, 0), lend = 2, mar=c(2,2,2,2), mfrow=c(num.models+1, 1))
    separationplot.icews(pred=bma.pred, actual=y.out, newplot=F, heading="BMA", type="line")
    for(i in 1:num.models){
      separationplot.icews(pred=newdata[,i], actual=y.out, 
      					     newplot=F, heading=paste("Model", i), type="line")
    }

  }
  
  out <- list(bma.pred=bma.pred, bma.auc=bma.auc, models.auc=models.auc, 
      	 			 bma.brier=bma.brier, models.brier=models.brier)
  return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{BMA}
\keyword{ blah }
