##setting up a monte carlo simulation for discrete covariates and sparse effects


monte.func.uds<- function(N = 250, folds = 10, files){

source('/nfs/home/J/jgrimmer/het/SLF_round2.R')

len<- length


##the first is very simply a model that finds a slight variation in a discrete covariates
weight.mat<- matrix(NA, nrow=50, ncol=7)
mse.mat<- matrix(NA, nrow=50, ncol=9)
cor.mat<- array(NA, dim = c(9,9,50))

monte <- 1

N<- N
folds<- folds

treats<- t(rmultinom(N, prob = rep(1/47, 47), size = 1))[,1:46]

##alright, let's rank these from big to small

inter<- 0
negs <- rbinom(46, size = 1, prob = 0.5)
neg_val <- rnorm(46, mean = -1, sd = 0.1)
pos_val <- rnorm(46, mean = 1, sd = 0.1)
beta<-  ifelse(negs==1, neg_val, pos_val)#c(2, 1, 0.5, -1, -2, runif(41, -0.07, 0.07) )
covs<- cbind(rbinom(N, size = 1, prob = 0.4), rbinom(N, size = 1, prob = 0.6))
gamma<- c(0.5, -0.5)
#interact_t1<- c(-0.5, -0.75)
#interact_t2<- c(0.5, -0.75)
#interact_t3<- c(1, 0)
#interact_t4<- c(0.5, 0.5)
#interact_t5<- c(0.5, 0.75)
interact<- matrix(NA, nrow=46, ncol=2)
for(z in 1:46){
	nega<- rbinom(1, size = 1, prob = 0.5)
	negb<- rbinom(1, size = 1, prob = 0.5)
	interact[z, 1]<- ifelse(nega==1, runif(1, min = -1, max = -0.5), runif(1, min = 0.5, max = 1))
	interact[z, 2] <- ifelse(negb==1, runif(1, min = -1, max = -0.5), runif(1, min = 0.5, max = 1))
	}



model.mat<- model.matrix(~covs*treats)
coefs<- c(inter, gamma, beta)
for(z in 1:46){
	coefs<- c(coefs, interact[z,])
	}
	


eff<- model.mat%*%coefs

ys<- c()
for(z in 1:N){
	
	ys[z]<- rbinom(1, prob = pnorm(eff[z]), size = 1)
	}
	

part1<- supLearnFit(covs, treats, ys, nfold=folds, speed=F)

part2<- wrap.func(covs, ys, treats, covs, ys, treats, speed=F)

##so now, creating the possible strata

pot.covs<- matrix(NA, nrow=4, ncol=2)
pot.covs[1,]<- c(0,0)
pot.covs[2,]<- c(1,0)
pot.covs[3,]<- c(0,1)
pot.covs[4,]<- c(1,1)

pos.treats<- matrix(0, nrow=46, ncol=46)
diag(pos.treats)<- 1


final.out<- matrix(NA, nrow=184, ncol=141)
final.out[,1]<- 1
a<- 0

seq1<- seq(50, 141, by=2)
seq2<- seq(51, 141, by=2)
for(z in 1:nrow(pos.treats)){
	for(j in 1:nrow(pot.covs)){
	a<- a + 1
	final.out[a,2:3]<- pot.covs[j,] 
	final.out[a,4:49]<- pos.treats[z,]
	for(m in 1:ncol(treats)){
	final.out[a,seq1[m]:seq2[m]]<- pos.treats[z,m]*pot.covs[j,]}
	}
	}

new.treats<- final.out#[1:20,]	
colnames(new.treats)<- colnames(model.mat)
colnames(new.treats)<- gsub('covs', 'X', colnames(new.treats))
colnames(new.treats)<- gsub('treats', 'treat', colnames(new.treats))

new.conts<- new.treats
new.conts[,c(4:141)]<- 0
colnames(new.conts)<- colnames(model.mat)
colnames(new.conts)<- gsub('covs', 'X', colnames(new.conts))
colnames(new.conts)<- gsub('treats', 'treat', colnames(new.conts))
preds<- predict.weight(part1, part2, new.treats, new.conts, model.mat, ys)


treat.eff<- preds$Treat - preds$Control
true.eff<- pnorm(new.treats%*%coefs) - pnorm(new.conts%*%coefs)


final.est<- c()
for(z in 1:nrow(preds$Treat)){
	final.est[z]<- sum((preds$Treat[z, ] - preds$Control[z,])*preds$Weight)}

mses<- c()
for(z in 1:ncol(treat.eff)){
	mses[z]<- sqrt( sum( (treat.eff[,z] - true.eff)^2)/nrow(treat.eff))
	}
mses[ncol(treat.eff) + 1]<- sqrt(sum( (final.est - true.eff)^2)/nrow(treat.eff))


flat<- apply(preds$Treat - preds$Control, 1, mean)

mses[ncol(treat.eff) + 2]<- sqrt(sum( (flat - true.eff)^2)/nrow(treat.eff))


mse.mat[monte,]<- mses
weight.mat[monte,]<- preds$Weight

fins<- cbind(preds$Treat, final.est, flat)
cor.fins<- cor(fins)
cor.mat[,,z]<- cor.fins

print(monte)
print(mses)


output<- list(mse.mat, weight.mat)
names(output)<- c('mse', 'weight')
return(output)
save(output, file = files)
}
