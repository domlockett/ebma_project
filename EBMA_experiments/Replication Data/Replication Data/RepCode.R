#############
#############
###Replication file for Grimmer, Messing, and Westwood
###
###
#############
#############



##Creating Figure 1, analyzing simulations
##set working directory to where the simulations are located

len<- length

disc_sp<- disc_den<- cont_sp<- cont_den<- list()



for(z in 1:5){
	out<- 'test_udd_'
	new<- paste(paste(out, z, sep=''), '.RData', sep='')
	load(new)
	disc_den[[z]]<- output
}

for(z in 1:5){
	out<- 'test_uds_'
	new<- paste(paste(out, z, sep=''), '.RData', sep='')
	load(new)
	disc_sp[[z]]<- output
}

for(z in 1:5){
	out<- 'test_ucd_'
	new<- paste(paste(out, z, sep=''), '.RData', sep='')
	load(new)
	cont_den[[z]]<- output
}


for(z in 1:5){
	out<- 'test_ucs_'
	new<- paste(paste(out, z, sep=''), '.RData', sep='')
	load(new)
	cont_sp[[z]]<- output
}


mse_udd<- mse_uds<- mse_ucd<- mse_ucs<- matrix(NA, nrow = 5, ncol = 11)

calc_mse<- function(list){

	mses<- matrix(NA, nrow = len(list), ncol = 11)
	for(z in 1:len(list)){
	mses[z,]<- list[[z]]$mse

	}


return(mses)
}


mse_udd<- calc_mse(disc_den)
mse_uds<- calc_mse(disc_sp)
mse_ucd<- calc_mse(cont_den)
mse_ucs<- calc_mse(cont_sp)



##creating the table for the simulations
##coming up with the relative scores
rel_udd<- rel_uds<- rel_ucd<- rel_ucs<- matrix(NA, nrow = 5, ncol = 10)

for(z in 1:5){
	rel_udd[z,]<- mse_udd[z,-10]/mse_udd[z,10]
	rel_uds[z,]<- mse_uds[z,-10]/mse_uds[z,10]
	rel_ucd[z,]<- mse_ucd[z,-10]/mse_ucd[z,10]
	rel_ucs[z,]<- mse_ucs[z,-10]/mse_ucs[z,10]

	}


##making the relative comparison

mean_mse_udd<- apply(mse_udd, 2, mean)
mean_mse_uds<- apply(mse_uds, 2, mean)
mean_mse_ucd<- apply(mse_ucd, 2, mean)
mean_mse_ucs<- apply(mse_ucs, 2, mean)

rel_mean_mse_udd<- mean_mse_udd[-10]/mean_mse_udd[10]
rel_mean_mse_uds<- mean_mse_uds[-10]/mean_mse_uds[10]
rel_mean_mse_ucd<- mean_mse_ucd[-10]/mean_mse_ucd[10]
rel_mean_mse_ucs<- mean_mse_ucs[-10]/mean_mse_ucs[10]


mean_mse<- rbind(mean_mse_udd, mean_mse_uds, mean_mse_ucd, mean_mse_ucs)

overall_mean<- apply(mean_mse, 2, mean)

rel_overall_mean<- overall_mean[-10]/overall_mean[10]


##names list
##LASSO, elastic net 0.5, elastic net 0.25, FindIt, Bayes GLM, BART, Random Forest, KRLS, SVM-SMO

names<- c('LASSO', 'Elastic Net 0.5', 'Elastic Net 0.25', 'Find It', 'Bayesian GLM','BART', 'Random Forest', 'KRLS', 'SVM-SMO', 'Weighted Ensemble',  'Naive Average')


table.func<- function(vecs, names){
	for(z in 1:nrow(vecs)){
		start<- paste(names[z], round(vecs[z,1],2),  sep='&')
		for(j in 2:ncol(vecs)){
			start<- paste(start, round(vecs[z,j],2), sep='&')
		}
		start<- paste(start, '\\\\', sep='')
			cat(start, '\n')

		}
	
	}


results<- cbind(rel_mean_mse_uds,  rel_mean_mse_ucs, rel_mean_mse_udd, rel_mean_mse_ucd, rel_overall_mean)


##for the main table
table.func(results, names)

##now for the online appendix

table.func(t(mse_uds[,]), names)
table.func(t(mse_ucs[,]), names)
table.func(t(mse_udd[,]), names)
table.func(t(mse_ucd[,]), names)

table.func(t(mse_udd[,-10]), names)




avg_weight_uds<- avg_weight_ucs<- avg_weight_udd<- avg_weight_ucd<- matrix(NA, nrow = 5, ncol = 9)

for(z in 1:5){
	avg_weight_uds[z,]<- disc_sp[[z]]$weight
	avg_weight_udd[z,]<- disc_den[[z]]$weight
	avg_weight_ucs[z,]<- cont_sp[[z]]$weight
	avg_weight_ucd[z,]<- cont_den[[z]]$weight

	}



mses<- c(mse_uds[,1:9], mse_udd[,1:9], mse_ucs[,1:9], mse_ucd[,1:9])
weights<- c(avg_weight_uds, avg_weight_udd, avg_weight_ucs, avg_weight_ucd)


par(cex.lab = 2)
par(mar = c(6, 5, 3, 2))
plot(c(avg_weight_uds, avg_weight_udd, avg_weight_ucs, avg_weight_ucd)~c(mse_uds[,1:9], mse_udd[,1:9], mse_ucs[,1:9], mse_ucd[,1:9]), xlab = 'Root Mean Squared Error', ylab = 'Weight', xlim=c(0, 0.5), pch = 20, col=gray(0.5))
#lines(lowess( c(avg_weight_uds, avg_weight_udd, avg_weight_ucs, avg_weight_ucd)~c(mse_uds[,1:9], mse_udd[,1:9], mse_ucs[,1:9], mse_ucd[,1:9]), iter = 0))

store<- loess(weights~mses, span = 0.75)

xs<- sort(unique(mses))[1:181]

ys<- predict(store, newdata =xs)
ys[which(ys<0)]<- 0


lines(ys~xs, lwd = 3)


dev.copy(device=pdf, file='SimCorr.pdf', height = 6 , width = 6)
dev.off()


##########Experiment 1

##this analysis creates the main effects in Figure 2

load('Het_Experiment.RData')


names(svdat)[23:51] = c("preq1", "preq2", "preq3", 
		"nextc", "contr", "nextt", "treat",
		"approval", "therm", "fiscRespbl", 
		"bringMoneyEff", "passLegEff",
		"secReqMC", "likGetM", "daysGetM", "break", 
		"gender", "race", "byear", "ntvEnglish",
		"ideo3", "voted", "pid3", "pidCloser", "educ",
		"inc", "finalinst", "howLong", "comments")



approv = agrep("I pay attention", max.distance=.3,
		svdat$comments)
approv2 = agrep("I PAY ATTENTION", max.distance=.3,
		svdat$comments)
approv = c(approv,approv2)
#svdat$comments[-approv]
svdat = svdat[approv,]


svdat$cond.type[which(svdat$contr==1)] = "control" 
svdat$cond.type = relevel(factor(svdat$cond.type), ref="control") 
svdat$cond.money[which(svdat$contr==1)] = "control" 
svdat$cond.money = relevel(factor(svdat$cond.money), ref="control") 
svdat$cond.stage[which(svdat$contr==1)] = "control" 
svdat$cond.stage = relevel(factor(svdat$cond.stage), ref="control") 
svdat$cond.party[which(svdat$contr==1)] = "control" 
svdat$cond.party = relevel(factor(svdat$cond.party), ref="control") 
svdat$cond.alongWith[which(svdat$contr==1)] = "control" 
svdat$cond.alongWith = relevel(factor(svdat$cond.alongWith), ref="control") 
levels(svdat$cond.alongWith) = c("control", "alone", "w/ Dem", "w/ Rep") 

# Fix up pid3
svdat$pid3l = factor(c("Dem", "Rep", "Ind/Oth", "Ind/Oth")[svdat$pid3])
svdat$pid3l = relevel(svdat$pid3l, ref="Ind/Oth")




with<- rep(0, nrow(svdat))
with[grep('w/', as.character(svdat$cond.along))]<- 1


cons<- ifelse(svdat$ideo3<3, 1, 0)
lib<- ifelse(svdat$ideo3==4|svdat$ideo3==5, 1, 0)

##setting up the conditions
types<- sort(unique(as.character(svdat$cond.type)))
type.num<- match(svdat$cond.type, types)
number<- c('control', '$20 million', '$50 thousand')
amount.num<- match(svdat$cond.money, number)
request<- c('control', 'requested', 'secured', 'will request')
stage.num<- match(svdat$cond.stage, request)
party<- c('control', 'a Republican', 'a Democrat')
party.num<- match(svdat$cond.party, party)
along<- c('control', 'alone', 'w/ Rep', 'w/ Dem')
along.num<- match(svdat$cond.alongWith, along)

type_labs<- c('Planned\nParenthood', 'Parks', 'Gun Range', 'Fire\nDepartment', 'Police', 'Roads')
money_labs<- c('$50 thousand', '$50 million')
stage_labs<- c('Will Request', 'Request', 'Secured')
along_labs<- c('Alone', 'w/ Republican', 'w/Democrat')
party_labs<- c('Republican', 'Democrat')

make.eff<- function(dep, indep){
	part1<- lm(dep~indep)
	coefs<- part1$coef[2:len(part1$coef)]
	ses<- sqrt(diag(vcov(part1)))[2:len(part1$coef)]
	out<- rbind(coefs, ses)
	return(out)}

##create the arrows and add to the plot



add.arrow<- function(eff, ys, labs){
	mins<- c(eff[1,] - 1.96*eff[2,])
	center<- eff[1,]
	maxs<- c(eff[1,] + 1.96*eff[2,])
	for(z in 1:len(ys)){
		arrows(mins[z], ys[z], maxs[z], ys[z], len = 0 , lwd = 2)
		points(center[z], ys[z], pch=20, cex = 1.25)
		}
	axis(2, c(ys[1]:ys[len(ys)]), labs)
	}
	


par(mar = c(5, 7, 4, 2))
par(las = 1)


plot(c(0,1)~c(0,1), ylim=c(1, 16), xlim=c(-0.3, 0.6), xlab='Treatment Effect', ylab='', axes=F, frame.plot=F)
axis(1, c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
axis(3, c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))

for(z in 1:16){
	arrows(-1e10, z, 1e10, z, lty= 2, col=gray(0.8))
}

for(z in c(-0.4,-0.35,  -0.3, -0.25, -0.2,-0.15,  -0.1,-0.05, 0.05, 0.1,0.15,  0.2,0.25,  0.3,0.35, 0.4,0.45, 0.5, 0.55, 0.6)){
	arrows(z, -1e10, z, 1e10, lty=2, col=gray(0.8))}

abline(v = 0, lwd=3)

approve_bi<- ifelse(svdat$approval<3, 1, 0)


start1<- make.eff(approve_bi, svdat$cond.type)
add.arrow(start1, c(1:6), type_labs)
start2<- make.eff(approve_bi, svdat$cond.money)
add.arrow(start2, c(7:8), c('$20 million','$50 thousand'))

start3<- make.eff(approve_bi, svdat$cond.stage)[,c(3, 1, 2)]
add.arrow(start3, c(9:11), c('Will Request', 'Request', 'Secured'))

start4<- make.eff(approve_bi, svdat$cond.alongWith)
add.arrow(start4, c(12:14), c('Alone', 'w/ Dem', 'w/ GOP'))

start5<- make.eff(approve_bi, svdat$cond.party)
add.arrow(start5, c(15:16), c('Democrat', 'Republican'))


dev.copy(device=pdf, file='MainEffects.pdf', height=8, width=6)
dev.off()





##now putting together the analysis of the first experiment


##building the first treatment matrix.  

type.mat<- matrix(0, nrow = 1074, ncol=7)
colnames(type.mat)<- sort(unique(as.character(svdat$cond.type)))
for(z in 1:nrow(type.mat)){
	type.mat[z,which(colnames(type.mat)==svdat$cond.type[z])]<- 1}
	
type.mat.final<- type.mat[,-1]

num.mat<- matrix(0, nrow=1074, ncol=3)
colnames(num.mat)<- number
for(z in 1:nrow(num.mat)){
	num.mat[z,which(colnames(num.mat)==svdat$cond.money[z])]<- 1
	}
num.mat.final<- num.mat[,-1]

stage.mat<- matrix(0, nrow=1074, ncol=4)
colnames(stage.mat)<- request
for(z in 1:nrow(stage.mat)){
	stage.mat[z,which(colnames(stage.mat)==svdat$cond.stage[z])]<- 1
	}
	
stage.mat.final<- stage.mat[,-1]

party.mat<- matrix(0, nrow=1074, ncol=3)
colnames(party.mat)<- party
for(z in 1:nrow(party.mat)){
	party.mat[z, which(colnames(party.mat)==svdat$cond.party[z])]<- 1
	}
	
party.mat.final<- party.mat[,-1]	
	
along.mat<- matrix(0, nrow=1074, ncol=4)
colnames(along.mat)<- 	along
for(z in 1:nrow(along.mat)){
	along.mat[z,which(colnames(along.mat)==svdat$cond.alongWith[z])]<- 1
	}

along.mat.final<- along.mat[,-1]	
	
	

##putting together the matrices

treat.mat<- cbind(type.mat.final, num.mat.final, stage.mat.final, party.mat.final, along.mat.final)

dem<- ifelse(svdat$pid3l=='Dem', 1, 0)
dem[which(is.na(dem))]<- 0
rep<- ifelse(svdat$pid3l=='Rep', 1, 0)
rep[which(is.na(rep))]<- 0
lib[which(is.na(lib))]<- 0
cons[which(is.na(cons))]<- 0

covs<- cbind(dem, rep, lib, cons)
colnames(covs)<- c('Dem', 'Rep', 'Lib', 'Cons')

##alright, now putting this together---putting together the interactions
##we want 
type.mat.final<- data.frame(type.mat.final)
colnames(type.mat.final)<- c('PlanParent', 'Parks', 'Gun_Range', 'Fire', 'Police', 'Roads')
num.mat.final<- data.frame(num.mat.final)
colnames(num.mat.final)<- c('mil_20', 'thou_50')
stage.mat.final<- data.frame(stage.mat.final)
colnames(stage.mat.final)<- c('request', 'secure', 'will')
party.mat.final<- data.frame(party.mat.final)
colnames(party.mat.final)<- c('rep_rep', 'dem_rep')

along.mat.final<- data.frame(along.mat.final)
colnames(along.mat.final)<- c('alone', 'w_rep', 'w_dem')


treats<- cbind(type.mat.final, num.mat.final[,1], stage.mat.final[,1:2],party.mat.final[,1], along.mat.final[,1:2], type.mat.final[,1:5]*num.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,2], type.mat.final[,1:5]*party.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,2], num.mat.final[,1]*stage.mat.final[,1], num.mat.final[,1]*stage.mat.final[,2], num.mat.final[,1]*party.mat.final[,1], num.mat.final[,1]*along.mat.final[,1], num.mat.final[,1]*along.mat.final[,2],stage.mat.final[,1:2]*party.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,2], party.mat.final[,1]*along.mat.final[,1], party.mat.final[,1]*along.mat.final[,2] )

type.short<- c('PlanParent', 'Parks', 'Gun_Range', 'Fire', 'Police', 'Roads')
num.short<- c('mil_20', 'thou_50')
stage.short<- c('request', 'secure', 'will')
party.short<- c('rep_rep', 'dem_rep')
along.short<- c('alone', 'w_rep', 'w_dem')


colnames(treats)[1:12]<- c(type.short, num.short[1], stage.short[1:2], party.short[1], along.short[1:2])
colnames(treats)[13:17]<- paste(type.short[1:5], num.short[1], sep='_x_')
colnames(treats)[18:22]<- paste(type.short[1:5], stage.short[1], sep='_x_')
colnames(treats)[23:27]<- paste(type.short[1:5], stage.short[2], sep='_x_')
colnames(treats)[28:32]<- paste(type.short[1:5], party.short[1], sep='_x_')
colnames(treats)[33:37]<- paste(type.short[1:5], along.short[1], sep='_x_')
colnames(treats)[38:42]<- paste(type.short[1:5], along.short[2], sep='_x_')
colnames(treats)[43]<- paste(num.short[1], stage.short[1], sep='_x_')
colnames(treats)[44]<- paste(num.short[1], stage.short[2], sep='_x_')
colnames(treats)[45]<- paste(num.short[1], party.short[1], sep='_x_')
colnames(treats)[46]<- paste(num.short[1], along.short[1], sep='_x_')
colnames(treats)[47]<- paste(num.short[1], along.short[2], sep='_x_')
colnames(treats)[48:49]<- paste(stage.short[1:2], party.short[1], sep='_x_')
colnames(treats)[50:51]<- paste(stage.short[1:2], along.short[1], sep='_x_')
colnames(treats)[52:53]<- paste(stage.short[1:2], along.short[2], sep='_x_')
colnames(treats)[54]<- paste(party.short[1], along.short[1], sep='_x_')
colnames(treats)[55]<- paste(party.short[2], along.short[2], sep='_x_')



##now sourcing the function
source('SLF_round2.R')

##now running the super learning
part1<- supLearnFit(covs, treats, approve_bi, 10, F)

part2<- wrap.func(covs, approve_bi, treats, covs, approve_bi, treats, speed=F)

##predictions are loaded here.  
load('HetExperimentPreds.RData')

diff<- preds$Treated - preds$Control

final.eff<- c()
for(z in 1:nrow(preds$Treated)){
	final.eff[z]<- diff[z,]%*%preds$Weight
	}
	


X<- covs
treat<- treats


model.mat<- model.matrix(~X*treat)

hyp.type<- matrix(0, nrow=6, ncol=6)

diag(hyp.type)<- 1
num.type<- c(1,0)
stage.type<- matrix(0, nrow=3, ncol=2)
diag(stage.type)<- 1
party.type<- c(1,0)
along.type<- matrix(0, nrow=3, ncol=2)
diag(along.type)<- 1

cov_party<- matrix(0, nrow=3, ncol=2)
diag(cov_party)<- 1

cov_ideo<- matrix(0, nrow=3, ncol=2)
diag(cov_ideo)<- 1


hyp.treat.mat<- matrix(0, nrow = 1944, ncol=279)

a<- 0
for(aa in 1:6){
	for(bb in 1:2){
		for(cc in 1:3){
			for(dd in 1:2){
				for(ee in 1:3){
					for(ff in 1:3){
						for(gg in 1:3){
							a <- a + 1
							part1_covs<- c(cov_party[ff,], cov_ideo[gg,])
							part1_treats<- c(hyp.type[aa,], num.type[bb], stage.type[cc,], party.type[dd], along.type[ee,])
							treat.inter<- c()
							p1tt<- part1_treats[-6]
							treat.inter<- c(treat.inter, hyp.type[aa,-6]*num.type[bb], hyp.type[aa,-6]*stage.type[cc,1], hyp.type[aa,-6]*stage.type[cc,2], hyp.type[aa,-6]*party.type[dd], hyp.type[aa,-6]*along.type[ee,1], hyp.type[aa,-6]*along.type[ee,2], num.type[bb]*stage.type[cc,1], num.type[bb]*stage.type[cc,2], num.type[bb]*party.type[dd], num.type[bb]*along.type[ee,1], num.type[bb]*along.type[ee,2], stage.type[cc,]*party.type[dd], stage.type[cc,]*along.type[ee,1], stage.type[cc,]*along.type[ee,2], party.type[dd]*along.type[ee,1], party.type[dd]*along.type[ee,2])
							hyp.treat.mat[a,1:59]<- c(part1_covs, part1_treats, treat.inter)
							cov.inter<- c()
							cols<- 60
							comp_treat<- c(part1_treats, treat.inter)
							for(hh in 1:len(comp_treat)){
									cov.inter<- c(cov.inter, comp_treat[hh]*cov_party[ff,], comp_treat[hh]*cov_ideo[gg,])
									}
									hyp.treat.mat[a,60:ncol(hyp.treat.mat)]<- cov.inter
								}
							}
							}}}}}
							
							
final.mat<- cbind(1, hyp.treat.mat)
cont.mat<- final.mat
cont.mat[,5:ncol(final.mat)]<- 0


X<- covs
treat<- as.matrix(treats)


model.mat<- model.matrix(~X*treat)
colnames(model.mat)<- gsub('X', '', colnames(model.mat))

treat.mat<- final.mat
colnames(treat.mat)<- colnames(cont.mat)<- colnames(model.mat)




create.plot4<- function(var1, pos.values_var1, labels_1, var2, pos.values, vert.label, xlim=c(-0.4, 0.4), left = 6, right= 6, mult, multiplier){
	num.facts<- len(pos.values)
	num.right<- len(pos.values_var1)
	fill<- matrix(0, nrow=num.right, ncol=num.facts)
	tt<- 1:nrow(treat.mat)
		av.effect<- function(subset){
	out<- mean(final.eff[subset])
	print(out)
	}
	num_category<- matrix(NA, nrow = num.right, ncol = num.facts)
	
	for(z in 1:num.right){
		for(k in 1:num.facts){
					
					fill[z, k]<- av.effect(which(var1==pos.values_var1[z] & var2==pos.values[k]))
					num_category[z,k]<- multiplier[pos.values[k]]
				# if(z ==2){		
					# fill[z, k]<- av.effect(which(treat.mat$treatCond2==1 & var2==pos.values[[k]]))
					# }
				# if(z ==3){		
					# fill[z, k]<- av.effect(which(treat.mat$treatCond3==1 & var2==pos.values[[k]]))
					# }
				# if(z ==4){		
					# fill[z, k]<- av.effect(which(treat.mat$treatCond4==1 & var2==pos.values[[k]]))
					# }
				# if(z ==5){		
					# fill[z, k]<- av.effect(which(treat.mat$treatCond5==1 & var2==pos.values[[k]]))
					# }
				# if(z ==6){		
					# fill[z, k]<- av.effect(which(treat.mat$treatCond6==1 & var2==pos.values[[k]]))
					# }
				}
			}
		#fill2<- fill[c(3, 1, 6, 5, 4, 2),]
		
		
	#	rank.fills<- apply(fill2, 2, rank.func)
		fill2<- fill
		par(mar=c(4, left, 3, right))
		par(las=1)	
		par(cex.lab = 1.25)
		plot(c(0,1)~c(0,1), pch='', xlab='Treatment Effect', ylab='', axes = F, ylim=c(1, num.right*num.facts), xlim= xlim, frame.plot=F)	
		
		a<- 0
		seqs<- seq(xlim[1], xlim[2], by=0.025)
		for(aa in 1:len(seqs)){
			arrows(seqs[aa], -10, seqs[aa], 1e7, len=0, lty=2, col=gray(0.6))
			}
		seq2<- seq(1, num.right*num.facts)	
		for(aa in 1:len(seq2)){
			arrows(-10, aa, 10, aa, len=0, col=gray(0.8))
			}
		arrows(0, -10, 0, 1e7, len=0, lwd=2)
		axis(1, seq(round(xlim[1], 2), round(xlim[2],2), by=0.05))
		axis(3, seq(round(xlim[1], 2), round(xlim[2],2), by=0.05))
		base.lab<-  labels_1
		labs<- c()
		labs2<- c()
		a<- 0
		x_store<- c()
		for(z in 1:num.right){
			ars<- ps<- c()
			for(k in 1:num.facts){
				a<- a + 1
				ars<- c(ars, a)
				points(fill2[z,k], a, pch=20, cex = num_category[z,k]*mult)
				x_store[a]<- fill2[z,k]
				ps<- c(ps, fill2[z,k])
				labs<- c(labs, vert.label[k])
				labs2<- c(labs2, paste(vert.label[k], base.lab[z], sep='_'))
				}
				points(ps, ars, type='l', lwd=1.25)
				}
		axis(2, 1:(num.right*num.facts), labs)
		count<- 1
		for(z in 1:num.right){
				seqs<- count:(count + num.facts - 1)
				axis(4, c(count , median(seqs),  count + num.facts-1), c('', base.lab[z], ''))
				count<- count + num.facts  
				}
				
						
names(x_store)<- labs2
return(x_store)
						
}

treat.mat<- as.data.frame(treat.mat)

treat.var<- rep(0, nrow(treat.mat))
treat.var[which(treat.mat$treatPlanParent==1)]<- 1
treat.var[which(treat.mat$treatParks==1)]<- 2
treat.var[which(treat.mat$treatGun_Range==1)]<- 3
treat.var[which(treat.mat$treatFire==1)]<- 4
treat.var[which(treat.mat$treatPolice==1)]<- 5
treat.var[which(treat.mat$treatRoads==1)]<- 6


var5<- rep(0, nrow(treat.mat))
var5[which(treat.mat$Lib==1 & treat.mat$Dem==1)]<- 1
var5[which(treat.mat$Lib==0 & treat.mat$Cons==0 & treat.mat$Dem==1)]<- 2
var5[which(treat.mat$Lib==0 & treat.mat$Cons==1 & treat.mat$Dem==1)]<- 3

var5[which(treat.mat$Lib==1 & treat.mat$Dem==0 & treat.mat$Rep==0)]<- 4
var5[which(treat.mat$Lib==0 & treat.mat$Cons==0 & treat.mat$Dem==0 & treat.mat$Rep==0)]<- 5
var5[which(treat.mat$Lib==0 & treat.mat$Cons==1 & treat.mat$Dem==0 & treat.mat$Rep==0)]<- 6

var5[which(treat.mat$Lib==1 & treat.mat$Rep==1)]<- 7
var5[which(treat.mat$Lib==0 & treat.mat$Cons==0 & treat.mat$Rep==1)]<- 8
var5[which(treat.mat$Lib==0 & treat.mat$Cons==1 & treat.mat$Rep==1)]<- 9

var6<- ifelse(treat.mat$treatmil_20==1 , 1, 2)



treat.var2<- rep(0, len(var5))
treat.var2[which(treat.mat$treatPlanParent==1 & treat.mat$treatmil_20==0)]<- 1
treat.var2[which(treat.mat$treatPlanParent==1 & treat.mat$treatmil_20==1)]<- 2

treat.var2[which(treat.mat$treatParks==1 & treat.mat$treatmil_20==0)]<- 3
treat.var2[which(treat.mat$treatParks==1 & treat.mat$treatmil_20==1)]<- 4


treat.var2[which(treat.mat$treatGun_Range==1 & treat.mat$treatmil_20==0)]<- 5
treat.var2[which(treat.mat$treatGun_Range==1 & treat.mat$treatmil_20==1)]<- 6


treat.var2[which(treat.mat$treatFire==1 & treat.mat$treatmil_20==0)]<- 7
treat.var2[which(treat.mat$treatFire==1 & treat.mat$treatmil_20==1)]<- 8


treat.var2[which(treat.mat$treatPolice==1 & treat.mat$treatmil_20==0)]<- 9
treat.var2[which(treat.mat$treatPolice==1 & treat.mat$treatmil_20==1)]<- 10

treat.var2[which(treat.mat$treatRoads==1 & treat.mat$treatmil_20==0)]<- 11
treat.var2[which(treat.mat$treatRoads==1 & treat.mat$treatmil_20==1)]<- 12



treat.var3<- rep(0, len(treat.var2))

treat.var3[which(treat.mat$treatPlanParent==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 1
treat.var3[which(treat.mat$treatPlanParent==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 2

treat.var3[which(treat.mat$treatPlanParent==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 3



treat.var3[which(treat.mat$treatParks==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 4
treat.var3[which(treat.mat$treatParks==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 5
treat.var3[which(treat.mat$treatParks==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 6




treat.var3[which(treat.mat$treatGun_Range==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 7
treat.var3[which(treat.mat$treatGun_Range==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 8
treat.var3[which(treat.mat$treatGun_Range==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 9




treat.var3[which(treat.mat$treatFire==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 10
treat.var3[which(treat.mat$treatFire==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 11
treat.var3[which(treat.mat$treatFire==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 12


treat.var3[which(treat.mat$treatPolice==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 13
treat.var3[which(treat.mat$treatPolice==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 14
treat.var3[which(treat.mat$treatPolice==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 15


treat.var3[which(treat.mat$treatRoads==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==0)]<- 16
treat.var3[which(treat.mat$treatRoads==1 & treat.mat$treatrequest==1 & treat.mat$treatsecure==0)]<- 17
treat.var3[which(treat.mat$treatRoads==1 & treat.mat$treatrequest==0 & treat.mat$treatsecure==1)]<- 18




new_labels<- c()
a<- 0
for(z in c('Planned\nParenthood', 'Parks', 'Gun\nRange', 'Fire\nDepartment', 'Police', 'Roads')){
	for(k in c('$50 Thousand', '$20 Million')){
		a<- a + 1
		new_labels[a]<- paste(z, k, sep='\n')
		}
	}
	
ideo_vec<- ifelse(cons==1, 'Conservative', ifelse(lib==1, 'Liberal', 'Moderate'))

store_tabs<- table(ideo_vec, svdat$pid3l)/nrow(svdat)

cbind(1:9, c('Liberal Democrat', 'Moderate Democrat', 'Conservative Democrat', 'Liberal Independent', 'Moderate Independent', 'Conservative Independent', 'Liberal Republican', 'Moderate Republican', 'Conservative Republican'))

store_vec<- rep(NA, 9)
store_vec[1]<- store_tabs[2,2]
store_vec[2]<- store_tabs[3,2]
store_vec[3]<- store_tabs[1,2]
store_vec[4]<- store_tabs[2,1]
store_vec[5]<- store_tabs[3,1]
store_vec[6]<- store_tabs[1,1]
store_vec[7]<- store_tabs[2,3]
store_vec[8]<- store_tabs[3,3]
store_vec[9]<- store_tabs[1,3]






var7<- ifelse(treat.mat$treatsecure==0 & treat.mat$treatrequest==0 , 1, ifelse(treat.mat$treatrequest==1, 2, 3))


new_labels3<- c()
a<- 0 
for(z in c('Planned\nParenthood', 'Parks', 'Gun\nRange', 'Fire\nDepartment', 'Police', 'Roads')){
	for(k in c("Will\nRequest", "Request", "Secured")){
		a<- a + 1
		new_labels3[a]<- paste(z, k, sep='\n')
		}
	}
	



store<- create.plot4(treat.var, 1:6, c('Planned\nParenthood', 'Parks', 'Gun\nRange', 'Fire\nDepartment', 'Police', 'Roads'), var5, 1:9, c('Liberal Democrat', 'Moderate Democrat', 'Conservative Democrat', 'Liberal Independent', 'Moderate Independent', 'Conservative Independent', 'Liberal Republican', 'Moderate Republican', 'Conservative Republican'), left=11, right=6, mult = 10, multiplier = store_vec)
dev.copy(device=pdf, file='MainFigure1.pdf', height = 10, width = 11)
dev.off()


mults<- table(ideo_vec)/nrow(svdat)
mults2<- mults[c(2, 3, 1)]

ideo_vec2<- ifelse(treat.mat$Lib==1, 1, ifelse(treat.mat$Cons==1, 2, 3))
create.plot4(treat.var2, 1:12, new_labels,	 ideo_vec2, 1:3, c('Liberal', 'Moderate', 'Conservative'), mult = 4,multiplier = mults2 )
dev.copy(device=pdf, file='SmallTypeMoneyIdeology.pdf', height = 12, width = 7)
dev.off()


create.plot4(treat.var3, 1:18, new_labels3, ideo_vec2, 1:3, c('Liberal', 'Moderate', 'Conservative'), mult = 4,multiplier = mults2)


dev.copy(device=pdf, file='SmallTypeSecuredIdeology_1.pdf', height = 12, width = 7)

dev.off()


####this replicates the analysis for Table 6 and Figure 5

data<-  read.delim("Public_v_Private_and_Taunting_2.csv", sep=',')



len <- length

subset<- data[which(data$creditCondition==1|data$creditCondition==2|data$creditCondition==3),]


dem<- ifelse(subset$Q67==1, 1,0)
rep<- ifelse(subset$Q67==2, 1, 0)
ind<- ifelse(subset$Q67==3, 1, 0)

strong_rep<- strong_dem<- rep(0, len(dem))

strong_rep[which(rep==1)]<- ifelse(subset$Q72[which(rep==1)]==1, 1, 0)
strong_dem[which(dem==1)]<- ifelse(subset$Q73[which(dem==1)]==1, 1, 0)



female<- ifelse(subset$Q74==2, 1, 0)

white<- ifelse(subset$Q75==1, 1, 0)

age<- subset$Q76 + 17

strong_lib<- ifelse(subset$Q78==5, 1, 0)
lib<- ifelse(subset$Q78==4 , 1, 0)
cons<- ifelse(subset$Q78==2, 1, 0)
mod<- ifelse(subset$Q78==3, 1, 0)
strong_cons<- ifelse(subset$Q78==1, 1, 0)

lib_dem_s<- ifelse(strong_lib==1 & strong_dem==1, 1, 0)
cons_rep_s<- ifelse(strong_cons==1 & strong_rep==1, 1, 0)


low_edu<- ifelse(subset$Q81<3, 1, 0)
med_edu<- ifelse(subset$Q81>2 & subset$Q81<6, 1, 0)
high_edu<- ifelse(subset$Q81>4, 1, 0)

low_inc<- ifelse(subset$Q82<4, 1, 0)
med_inc<- ifelse(subset$Q82>3 & subset$Q82<7, 1, 0)
high_inc<- ifelse(subset$Q82>6, 1, 0)

age_1<- ifelse(age<37, 1, 0)
age_2<- ifelse(age>36 & age<50, 1, 0)
age_3<- ifelse(age>49 & age<64, 1, 0)
age_4<- ifelse(age>63, 1, 0)

leg.est<- read.delim('JackmanEstimates.csv', sep=',')




c112_lnames<- tolower(as.character(leg.est[,1]))


rep_ideals<- rep(NA, nrow(subset))

missing<- c()

double<- c()
for(z in 1:nrow(subset)){
	match<- which(c112_lnames==tolower(subset$lastName)[z] & as.character(leg.est[,3])==as.character(subset$state)[z])
	if(len(match)==1){
		rep_ideals[z]<- leg.est[match,10]
		}
	if(len(match)==0){
		missing<- c(missing, z)
		}
	if(len(match)>1){
	test<- leg.est[match,]
	fnames<- tolower(as.character(test[,2]))
	ee<- which(fnames==subset$firstName[z])
	if(len(ee)>0){
		rep_ideals[z]<- leg.est[match[ee],10]
		}
	}
	}
	
##ok, now putting it together


rep_dem<- ifelse(subset$party=='D', 1, 0)

approve_bi<- ifelse(subset$Q17<3, 1, 0)
treat<- factor(subset$creditCondition)
match<- ifelse((dem==1 & rep_dem==1) | (rep==1 & rep_dem==0), 1, 0)

rep_match<- ifelse(rep==1 & rep_dem==0, 1, 0)
dem_match<- ifelse(dem==1 & rep_dem==1, 1, 0)


##alright, putting everything together now.  we'll have demographic characteristics, political characteristics and their interaction



covs<- cbind(low_edu, med_edu, low_inc, med_inc, age_1, age_2, age_3, female, white, strong_lib, lib, cons, strong_cons, strong_dem, dem, ind, rep, strong_rep, rep_dem, dem_match, rep_match, rep_ideals, abs(rep_ideals), strong_lib*dem_match, strong_cons*rep_match, dem_match*rep_ideals, rep_match*rep_ideals)
colnames(covs)[23:27]<- c('|repIdeals|',  'strongLib_x_demMatch', 'strongCons_x_repMatch', 'demMatch_x_repIdeals', 'repMatch_x_repIdeals')



treat.mat<- matrix(0, nrow=nrow(covs), ncol=2)
for(z in 1:nrow(treat.mat)){
	treat.mat[z,1]<- ifelse(treat[z]==2, 1, 0)
	treat.mat[z,2]<- ifelse(treat[z]==3, 1, 0)
	}

colnames(treat.mat)<- c('CBO', 'Partisan')
	
rms<- na.omit(cbind(covs, treat.mat, approve_bi))

covs<- rms[,1:27]
treats<- rms[,28:29]
app<- rms[,30]	
	
X<- covs
treat<- treats	
	
model.mat<- model.matrix(~X*treat)	


##putting together the matrix for estimating effects
edu_mat<- matrix(0, nrow=3, ncol=2)
diag(edu_mat)<- 1

party_id_mat<- matrix(0, nrow=4, ncol=3)
party_id_mat[1,1]<- party_id_mat[2,2]<- party_id_mat[3,3]<- 1


strong_mat<- c(1,0)


inc_mat<- matrix(0, nrow=3, ncol=2)
diag(inc_mat)<- 1

ideo_mat<- matrix(0, nrow=5, ncol=4)
diag(ideo_mat)<- 1

fem_mat<- c(1,0)

age_mat<- matrix(0, nrow=4, ncol=3)
diag(age_mat)<- 1


white_mat<- c(1,0)


treat.counter<- matrix(0, nrow=2, ncol=2)
diag(treat.counter)<- 1

party_id_mat<- matrix(0, nrow=4, ncol=3)
diag(party_id_mat)<- 1

rep_dem_mat<- c(1,0)

ideals<- c(-1.9, -1, 0.02, 0.11, 0.9, 1.5)


treat.mat<- cont.mat<- matrix(NA, nrow=51840, ncol=83)
a<- 0 
for(aa in 1:nrow(edu_mat)){
	for(bb in 1:nrow(inc_mat)){
		for(cc in 1:nrow(age_mat)){
			for(dd in 1:len(fem_mat)){
				for(ee in 1:len(white_mat)){
					for(ff in  1:nrow(ideo_mat)){
						for(gg in 1:nrow(party_id_mat)){
							if(party_id_mat[gg,1]==1|party_id_mat[gg,3]==1){
								strong_mat<- c(1,0)}
							else{
								strong_mat<- 0}
							for(hh in 1:len(strong_mat)){
								for(ii in 1:len(rep_dem_mat)){
									for(jj in 1:3){
										for(kk in 1:2){
											a<- a + 1
											ins_ideal<- ifelse(rep_dem_mat[ii]==1, ideals[1:3][jj], ideals[4:6][jj])
											abs_ind_ideal<- abs(ins_ideal)
											r_m_ins<- ifelse(party_id_mat[gg,3]==1 & rep_dem_mat[ii]==0, 1, 0)
											d_m_ins<- ifelse(party_id_mat[gg,1]==1 & rep_dem_mat[ii]==1, 1, 0)
											sd_ins<- ifelse(party_id_mat[gg,1]==1, strong_mat[hh], 0)
											sr_ins<- ifelse(party_id_mat[gg,3]==1, strong_mat[hh], 0)
											part_ab<- c(edu_mat[aa,], inc_mat[bb,], age_mat[cc,], fem_mat[dd], white_mat[ee], ideo_mat[ff,], sd_ins, party_id_mat[gg,], sr_ins, rep_dem_mat[ii], d_m_ins, r_m_ins, ins_ideal, abs_ind_ideal, ideo_mat[ff,1]*d_m_ins, ideo_mat[ff,4]*r_m_ins, d_m_ins*ins_ideal, r_m_ins*ins_ideal)
											treat.mat[a,1:27]<- part_ab
											treat.mat[a, 28:29]<- treat.counter[kk,]
											cols<- 30
											for(ll in 1:2){
												for(mm in 1:27){
											treat.mat[a,cols]<- treat.counter[kk,ll]*treat.mat[a,mm]
												cols<- cols + 1
												}}
											}}}}}}}}}}
											print(aa)}
										
cont.mat<- treat.mat

cont.mat[,28:83]<- 0									
									
cont.mat<- cbind(1, cont.mat)
treat.mat<- cbind(1, treat.mat)

colnames(treat.mat)<- colnames(cont.mat)<- colnames(model.mat)
colnames(treat.mat)<- gsub('X', '', colnames(treat.mat))
colnames(cont.mat)<-  gsub('X', '', colnames(cont.mat))


##this loads the predictions

load('BlamePreds.RData')



 diff<- preds$Treated - preds$Control

##the final heterogeneous estimates
 final.eff<- diff%*%preds$Weights
 
 ##this provides the most responsive subset
 
 
	 final.eff[order(final.eff, decreasing=T)[1:20]]
	 treat.mat[order(final.eff, decreasing=T)[1:20],]




seq1<- c(1:5)
seq2<- c(1:5)
mod2<- ifelse(apply(treat.mat[,11:14], 1, sum)==0, 1, 0)

ideos<- cbind(treat.mat[,11:12], mod2, treat.mat[,13:14])


name1<- c('StrongLib', 'Lib', 'Mod', 'Cons', 'StrongCons')
name2<- c('StrongDem', 'Dem', 'Ind', 'Rep', 'StrongRep')
name3<- c('Democratic\nRepresentative', 'Republican\nRepresentative')


strong_dem<- ifelse(treat.mat[,15]==1 & treat.mat[,16]==1, 1, 0)
dem<- ifelse(treat.mat[,15]==0 & treat.mat[,16]==1, 1, 0)
ind<- ifelse(treat.mat[,17]==1, 1, 0)
rep<- ifelse(treat.mat[,18]==1 & treat.mat[,19]==0, 1, 0)
strong_rep<- ifelse(treat.mat[,18]==1 & treat.mat[,19]==1, 1, 0)

pids<- cbind(strong_dem, dem, ind, rep, strong_rep)

new_label<- c()

treat.mat<- as.data.frame(treat.mat)

##doing same thing with other data

left_var<- right_var<- rep(0, nrow(treat.mat))
a<- 0 
for(z in seq1){
	for(y in seq2){
		a <- a + 1
		left_var[which(ideos[, z]==1 & pids[,y]==1)]<- a
		new_label[a]<- paste(name1[z], name2[y], sep='/')
		}
		}
		
right_var[which(treat.mat$rep_dem==1)]<- 1
right_var[which(treat.mat$rep_dem==0)]<- 2


dem<- ifelse(subset$Q67==1, 1,0)
rep<- ifelse(subset$Q67==2, 1, 0)
ind<- ifelse(subset$Q67==3, 1, 0)

strong_rep<- strong_dem<- rep(0, len(dem))

strong_rep[which(rep==1)]<- ifelse(subset$Q72[which(rep==1)]==1, 1, 0)
strong_dem[which(dem==1)]<- ifelse(subset$Q73[which(dem==1)]==1, 1, 0)



female<- ifelse(subset$Q74==2, 1, 0)

white<- ifelse(subset$Q75==1, 1, 0)

age<- subset$Q76 + 17

strong_lib<- ifelse(subset$Q78==5, 1, 0)
lib<- ifelse(subset$Q78==4 , 1, 0)
cons<- ifelse(subset$Q78==2, 1, 0)
mod<- ifelse(subset$Q78==3, 1, 0)
strong_cons<- ifelse(subset$Q78==1, 1, 0)


pid1<- cbind(strong_dem, dem, ind, rep, strong_rep)
ideo1<- cbind(strong_lib, lib,  mod, cons, strong_cons)

fill_dem<- fill_rep<- matrix(NA, nrow = 5, ncol = 5)

a<- 0
point_size<- rep(0, 25)
for(y in 1:5){
for(z in 1:5){
		a<- a + 1
		point_size[a]<- length(which(pid1[,z]==1 & ideo1[,y]==1))
	}
	}



point_size<- point_size/sum(point_size)

par(mfrow=c(1,1))

create.plot4(right_var, 1, name3, left_var, 1:26, new_label,xlim =c(-0.25, 0.25), left =10, right = 8, multiplier= rep(point_size, 2), mult = 50)
dev.copy(device=pdf, file='~/Dropbox/creditClaimingProjects/het/NewBlame1.pdf', height = 8, width = 6)
dev.off()



create.plot4(right_var, 2, name3[2], left_var, 1:26, new_label,xlim =c(-0.25, 0.25), left =10, right = 8, multiplier= rep(point_size, 2), mult = 50)

dev.copy(device=pdf, file='~/Dropbox/creditClaimingProjects/het/NewBlame2.pdf', height = 8, width = 6)
dev.off()










