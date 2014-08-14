#setting up rjags model

library(rjags)
dataset<-read.csv("dataset.csv")
series<-as.vector(unique(dataset$Series))
tau_params<-c(1/1000,1/100,1/14)
results_output<-matrix(nrow=0,ncol=6)
progress<-0

bayes_flux_model<-function(temp){

#source("/Users/ryanjwtx/Desktop/gassy_bayes/passable_model.R")

#establishing parameters
Ndata<-nrow(temp)
Time<-temp$Time
y<-temp$Concentration

#transforming parameters
zTime<-Time-mean(Time)
zy<-y-mean(y)

flux_model_imp<-jags.model(textConnection(pass_model), data=list("Ndata"=Ndata, "Time"=zTime, "y"=zy), n.chains=10)
chains<-coda.samples(model=flux_model_imp, c("beta1"),n.iter=400000,thin=200)
#plot(chains)
#autocorr.plot(chains, 50, auto.layout = FALSE)
results<-matrix(nrow=0,ncol=1)

for(i in 1:10){
	burned<-as.matrix(chains[[i]][1001:2000,],ncol=1)
	results<-rbind(results, burned)	
}
means<-mean(results)
medians<-median(results)
high95<-quantile(results,0.975)
low95<-quantile(results, 0.025)
result_stats<-c(means,medians,high95[[1]],low95[[1]])
return(result_stats)
}







for(j in 1:3){
	#j<-2
	tau_param<-tau_params[j]
	source("passable_model.R")
for(i in 1:length(series)){
	#i<-1
	temp<-subset(dataset, Series==series[i])
	#head(temp)
	stats<-bayes_flux_model(temp)
	func_output<-c(series[i],stats,tau_param)
	#func_output
	results_output<-rbind(results_output,func_output)
	
	if(progress > 250){
		print(progress/length(series))
		progress<-0
	}
	
}
}