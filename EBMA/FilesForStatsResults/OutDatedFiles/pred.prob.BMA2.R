### A new pred.prob.BMA() funciton using the Raferty Team Approach



pred.prob.BMA2 <-
function(y, x, control, training) {

y <- glm.model.econ$y
x <- icews


  
  # Read in data, and break it into test and training sets
  x.train<-x[training==1,]
  y.train<-y[training==1]
  x.test<-cbind(1, x[training==0,])
  y.test<-y[training==0]

