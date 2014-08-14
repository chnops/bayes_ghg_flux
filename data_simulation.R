# making simulated data for comparisons.  Simulating 1000 series of flux measurements for use in the HMR method
series<-matrix(nrow=0,ncol=1)


for(i in 1:1000){
	name<-sample(letters,6)
	name<-paste(name[1],name[2],name[3],name[4],name[5],name[6],sep="")
	names<-rbind(name,name,name,name)
	series<-rbind(series,names)
}
summary(series)
dim(series)
V<-rep(140.625,4000)
A<-rep(0.5625,4000)
Time<-rep(c(0,15,30,45),1000)
Concentration<-rnorm(4000,323,14)

dataset<-data.frame(series[,1],V,A,Time,Concentration)
names(dataset)[1]<-"Series"
head(dataset)

#these are packages used in this script

library(HMR)



# simulated dataset must be written to a .csv then navigate to that directory and run HMR code
#write.csv(dataset, "/Users/ryanjwtx/Desktop/gassy_bayes/dataset.csv",row.names=FALSE)
setwd("/Users/ryanjwtx/Desktop/gassy_bayes/")
dataset<-read.csv("dataset.csv")
hmr.results<-HMR("dataset.csv",sep=",",FollowHMR=TRUE, LR.always=TRUE)
hmr.results
hmr_results<-hmr.results[,c(1,2,4,7,9,11)]
write.csv(hmr_results, "1000_hmr_result.csv",row.names=FALSE)
hmr_results<-read.csv("1000_hmr_result.csv")



# if there are NA's for P-values, we converted them to a 1
for(i in 1:dim(hmr_results)[1]){
	
	if(is.na(hmr_results$f0.p[i])==TRUE){hmr_results$f0.p[i]<-1}
}
write.csv(hmr_results, "1000_hmr_result.csv",row.names=FALSE)


