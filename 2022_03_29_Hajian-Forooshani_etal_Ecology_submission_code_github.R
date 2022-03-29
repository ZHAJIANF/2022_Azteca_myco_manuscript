###########################################################################
############################### Figure 2 ##################################
###########################################################################

rm(list=ls()

data.raw <-read.csv("2022_03_29_Hajian-Forooshani_etal_Ecology_submission_Azteca-Myco_time_series_data.csv")

data <- data.raw

## number of resamples for the bootstrapping
num.resamples <- 500

## organize years and months for processing the time series 
year.vec <- sort(unique(data$year))
month.vec <- seq(1,12,1)

# collect outputs
azteca.mean.roya <- c()
azteca.error.roya <- c()
no.azteca.mean.roya <- c()
no.azteca.error.roya <- c()
year.id <- c()
month.id <- c()
num.nests <-c()

p.val.collector <- c()
real.diff.collector <- c()

## start of bootstrapping procedure
for(year.sel in 1:length(year.vec)){
	## need to account for year 1 not having full 12 months of data
	if(year.sel == 1){
		for( month.sel in 9:12){
		## year 1 starts at month 9
		#year.sel <- 1
		#month.sel <- 10
		year.dat <- data[data$year == year.vec[year.sel],] 
		year.month.dat <- year.dat[year.dat$month == month.vec[month.sel], ]

		az.dat <- year.month.dat[year.month.dat$azteca == "con" | year.month.dat$azteca == "vive" |  year.month.dat$azteca == "casi es muerto",]
		num.nests <-c(num.nests, length(unique(az.dat$placa)))

		no.az.dat <- year.month.dat[year.month.dat$azteca == "sin",]

		az.prop.roya.dirty <- az.dat$roya/az.dat$hojas
		no.az.prop.roya.dirty <- no.az.dat$roya/no.az.dat$hojas
		
		## now clean out NaNs and NAs
		az.prop.roya <- az.prop.roya.dirty[!is.na(az.prop.roya.dirty) & !is.nan(az.prop.roya.dirty) & az.prop.roya.dirty != Inf]
		no.az.prop.roya <- no.az.prop.roya.dirty[!is.na(no.az.prop.roya.dirty) & !is.nan(no.az.prop.roya.dirty) & no.az.prop.roya.dirty != Inf]
		
		########## Bootstrapping azteca/non-azteca
		null.diff.vec <- numeric(num.resamples)
		for(strappin in 1:num.resamples){
			
			joined.dat <- c(az.prop.roya, no.az.prop.roya)
			az.null.dat <- sample(joined.dat, size= length(az.prop.roya),replace=F)
			no.az.null.dat <- sample(joined.dat, size= length(no.az.prop.roya),replace=F)
			
			null.diff.vec[strappin] <- mean(az.null.dat) - mean(no.az.null.dat)
			
		}
		
		real.dif <- mean(az.prop.roya) - mean(no.az.prop.roya)
		real.diff.collector <- c(real.diff.collector ,real.dif)

		#hist(null.diff.vec)
		#abline(v=real.dif)
		p.val.collector <- c(p.val.collector, sum(null.diff.vec >= real.dif)/num.resamples )
		######### End of bootstrapping
		
		azteca.mean.roya <- c(azteca.mean.roya, mean(az.prop.roya)) 
		azteca.error.roya <- c( azteca.error.roya, sd(az.prop.roya)/sqrt(length(az.prop.roya)) )

		no.azteca.mean.roya <- c( no.azteca.mean.roya, mean(no.az.prop.roya) )
		no.azteca.error.roya <- c(no.azteca.error.roya, sd(no.az.prop.roya)/sqrt(length(no.az.prop.roya)) )
		year.id <- c(year.id,year.sel)
		month.id <- c(month.id, month.sel)
		} 
	}## end of year 1 with months 9-12
	
	if(year.sel > 1){
	for( month.sel in 1:12){
		## year 1 starts at month 9
		#year.sel <- 3
		#month.sel <- 10
		year.dat <- data[data$year == year.vec[year.sel],] 
		year.month.dat <- year.dat[year.dat$month == month.vec[month.sel], ]

		az.dat <- year.month.dat[year.month.dat$azteca == "con"| year.month.dat$azteca == "vive" |  year.month.dat$azteca == "casi es muerto",]
		num.nests <-c(num.nests, length(unique(az.dat$placa)))
		no.az.dat <- year.month.dat[year.month.dat$azteca == "sin",]


		az.prop.roya.dirty <- az.dat$roya/az.dat$hojas
		no.az.prop.roya.dirty <- no.az.dat$roya/no.az.dat$hojas
		
		## now clean out NaNs and NAs
		az.prop.roya <- az.prop.roya.dirty[!is.na(az.prop.roya.dirty) & !is.nan(az.prop.roya.dirty) & az.prop.roya.dirty != Inf ]
		no.az.prop.roya <- no.az.prop.roya.dirty[!is.na(no.az.prop.roya.dirty) & !is.nan(no.az.prop.roya.dirty) & no.az.prop.roya.dirty != Inf ]
		
		########## Bootstrapping azteca/non-azteca
		null.diff.vec <- numeric(num.resamples)
		for(strappin in 1:num.resamples){
			
			joined.dat <- c(az.prop.roya, no.az.prop.roya)
			az.null.dat <- sample(joined.dat, size= length(az.prop.roya),replace=F)
			no.az.null.dat <- sample(joined.dat, size= length(no.az.prop.roya),replace=F)
			
			null.diff.vec[strappin] <- mean(az.null.dat) - mean(no.az.null.dat)
			
		}
		
		real.dif <- mean(az.prop.roya) - mean(no.az.prop.roya)
		real.diff.collector <- c(real.diff.collector ,real.dif)
		
		#hist(null.diff.vec)
		#abline(v=real.dif)
		p.val.collector <- c(p.val.collector, sum(null.diff.vec >= real.dif)/num.resamples )
		######### End of bootstrapping		
		
		azteca.mean.roya <- c(azteca.mean.roya, mean(az.prop.roya)) 
		azteca.error.roya <- c( azteca.error.roya, sd(az.prop.roya)/sqrt(length(az.prop.roya)) )

		no.azteca.mean.roya <- c( no.azteca.mean.roya, mean(no.az.prop.roya) )
		no.azteca.error.roya <- c(no.azteca.error.roya, sd(no.az.prop.roya)/sqrt(length(no.az.prop.roya)) )
		
		year.id <- c(year.id,year.sel)
		month.id <- c(month.id, month.sel)
	}
	}

}## end of collecting azteca no azteca prop roya 



## get rid of time points with missing survey data
num.nests[num.nests == 0] <- NA

## plotting 
pick.lwd <- 2
pick.cex <- 0.1
p.threshold <- 0.05
par(mai=c(0.8,0.8,0.1,01))
plot(num.nests/128*100,col="goldenrod",yaxt="n",xaxt="n",ylab="",xlab="",pch=19,cex=0.01,ylim=c(0,30))
for(x in 1:length(p.val.collector)){
	#x <- 1
	if(p.val.collector[x] > (1-p.threshold) & !is.na(p.val.collector[x] )){
		max.yval <- max(c(azteca.mean.roya[x] + azteca.error.roya[x], no.azteca.mean.roya[x] + no.azteca.error.roya[x] ) )
		#text(x, 0.31,"-",col="black",cex=1)
		abline(v=x,lty=3,col="black")

	}
}
for(x in 1:length(p.val.collector)){
	#x <- 1
	if(p.val.collector[x] < p.threshold & !is.na(p.val.collector[x] )){
		max.yval <- max(c(azteca.mean.roya[x] + azteca.error.roya[x], no.azteca.mean.roya[x] + no.azteca.error.roya[x] ) )
		#text(x, 0.31,"+",col="black",cex=1)
		abline(v=x,lty=4,col="red")
	}
}
points(num.nests/128*100,col="goldenrod",pch=19)
axis(4,las=2)
points(num.nests/128*100,type="l",col="goldenrod",lwd=2)
par(new=T)
index <- seq(1,length(azteca.mean.roya),1)
plot(index ,azteca.mean.roya*100,pch=19,yaxt="n",xaxt="n",xlab="",ylab="",col="red",ylim=c(0,30),cex= pick.cex)

axis(2,las=2)
points(index ,azteca.mean.roya*100, type="l",col="red",lwd= pick.lwd)
arrows(index ,azteca.mean.roya*100 + azteca.error.roya*100 , index ,azteca.mean.roya*100 - azteca.error.roya*100 ,code=3,length= 0.02,angle=90,col="red",lwd= pick.lwd)
index2 <- index + 0.2
points(index2,no.azteca.mean.roya*100,col="black",pch=19,cex= pick.cex)
points(index2 ,no.azteca.mean.roya*100, type="l",col="black",lwd= pick.lwd)
arrows(index2 , no.azteca.mean.roya*100 + no.azteca.error.roya*100 , index2 , no.azteca.mean.roya*100 - no.azteca.error.roya*100 ,code=3,length=0.02,angle=90,col="black",lwd= pick.lwd)
mtext("% CLR infection",side=2,line=2.2)
mtext("% sites with ant nests",side=4,line=2.2)

## this centers the year label on june
year.indexes <- c(10,22,34,46,58,70,82)
year.names <- c("2014","2015","2016","2017",'2018',"2019","2020")
axis(1,labels= year.names, at= year.indexes,las=1)
#legend(75,0.7,legend=c("Azteca","No Azteca"),pch=19,col=c("red","black"),bty="n",title="")
#legend(71.5,34,legend=c("Ant sites","No ant sites","# ant nests"),pch=19,col=c("red","black","goldenrod"),bty="n",title="",bg="white",cex=0.75)


###########################################################################
############################### Figure 3 ##################################
###########################################################################

rm(list=ls())
data<- read.csv("2022_03_29_Hajian-Forooshani_etal_Ecology_submission_Azteca-Myco_lab_experiment_data.csv")

### looking at the change in both treatments after 24 hours of exposure to Azteca
cont.dif <- (data$c_myco_counted) - (data$initial_myco)
ant.dif <- (data$ant_myco)- (data$initial_myco)


## calculate the mean change in both treatments 
means <- c(mean(cont.dif), mean(ant.dif))
errors <- c(sd(cont.dif)/sqrt(length(cont.dif)),sd(ant.dif)/sqrt(length(ant.dif)) )
errors <- errors*1.96


## make plot that includes the individual replicates 
par(mfrow=c(1,1),mai=c(0.8,0.8,0.1,0.1))
index <- seq(1,2,1)
plot(means,ylim=c(-26,15),yaxt="n",xaxt="n",ylab="",xlab="",xlim=c(0.5,2.5),cex=0)
control.jit <- jitter(rep(1, length(cont.dif)),amount=0.3)
ant.jit <- jitter(rep(2, length(ant.dif)),amount=0.3)
for(x in 1:length(cont.dif)){
	segments(control.jit[x], cont.dif[x], ant.jit[x], ant.dif[x] ,col="black",lty=3,lwd=0.5)	
}
#arrows(index,means + errors,index,means-errors,code=3,length=0.02,angle=90)
axis(2,las=2)
#axis(1,labels= c("Control",expression(italic(Azteca))),at=index)
axis(1,labels= c("Control","Ant"),at=index)
points(control.jit, cont.dif,col="grey" ,lwd=1,pch=19)
points(ant.jit, ant.dif,col="red",lwd=1,pch=19 )
points(control.jit, cont.dif,col="grey" ,lwd=0.75,pch=1)
points(ant.jit, ant.dif,col="red",lwd= 0.75,pch=1 )
points(means,col=c("black","dark red"),pch=19)
arrows(index,means + errors,index,means-errors,code=3,length=0.02,angle=90,col=c("black","dark red"),lwd=2)
#mtext(expression(paste(Delta," " ,italic( Mycodiplosis)," larvae")),side=2,line=2.5)
mtext(expression(paste(Delta," CLR-midge larvae")),side=2,line=2.5)
abline(h=0,lty=2)
mtext("Treatment",side=1,line=2.5)


## Now test for signifiant difference between the treatments. 

## a wilcoxon rank sum test
wilcox.test(cont.dif, ant.dif)
#W = 2027, p-value = 0.007358

## bootstrapping the difference bewteen the two means 
real.diff <- means[1] - means[2] ## observed difference in means 

num.samples <- 10000
null.diffs <- numeric(num.samples)
null.df <- c(cont.dif,ant.dif)
set.seed(1)
for(xxx in 1:num.samples){
	null.cont <- sample(null.df,size=length(cont.dif),replace=T)
	null.ant <- sample(null.df,size=length(ant.dif),replace=T)
	
	null.diffs[xxx] <- mean(null.cont) - mean(null.ant)
}

hist(null.diffs,breaks=200,main="",xlab="",yaxt="n")
axis(2,las=2)
abline(v=real.diff,col="red",lty=2,lwd=1.5)
text(-3,140,paste("p=",sum(real.diff <= null.diffs )/num.samples),cex=0.75)
mtext("control - treatment means",side=1,line=2.5)


###########################################################################
######################## Supplementary material ###########################
###########################################################################


rm(list=ls())
data<- read.csv("2022_03_29_Hajian-Forooshani_etal_Ecology_submission_Azteca-Myco_lab_experiment_data.csv")

## looking for density dependence in the effect of Azteca on Mycodiplosis 
## calculate the proportion change for both treatments 

cont.dif.raw <- (data$c_myco_counted - data$initial_myco) / data$initial_myco
ant.dif.raw <- (data$ant_myco - data$initial_myco)/ data$initial_myco

inital.myco <- data$initial_myco
ant.dif <- ant.dif.raw
cont.dif <- cont.dif.raw

plot(NA,xlim=c(0,60),ylim=c(-1,3.5),xlab="",ylab="",yaxt="n")
points(inital.myco , cont.dif,col="grey",lwd=2)
points(inital.myco , ant.dif,col="red",lwd=2)
cont.model <- lm(cont.dif ~ inital.myco)
ant.model <-lm(ant.dif ~ inital.myco )
abline(cont.model,lwd=2,col="grey")
abline(ant.model,lwd=2,col="red")
summary(cont.model)
summary(ant.model)
mtext(expression(paste("prop ",Delta,"CLR-midge")),side=2,line=2.5)
axis(2,las=2)
mtext(expression(paste("Initial CLR-midge")),side=1,line=2.5)
segments(inital.myco, cont.dif, inital.myco, ant.dif)


## same thing but removing the outlier from ("newly discovered Mycodiplosis on leaves" -- see manuscript for details )
#### this still has no impact on the qualitive result of no density effect
inital.myco<- data$initial_myco[ant.dif.raw < 3]
ant.dif <-ant.dif.raw[ant.dif.raw < 3]
cont.dif <-cont.dif.raw[ant.dif.raw < 3]

plot(NA,xlim=c(0,60),ylim=c(-1,1),xlab="",ylab="",yaxt="n")
points(inital.myco , cont.dif,col="grey",lwd=2)
points(inital.myco , ant.dif,col="red",lwd=2)
cont.model <- lm(cont.dif ~ inital.myco)
ant.model <-lm(ant.dif ~ inital.myco )
abline(cont.model,lwd=2,col="grey")
abline(ant.model,lwd=2,col="red")
summary(cont.model)
summary(ant.model)
mtext(expression(paste("prop ",Delta,"CLR-midge")),side=2,line=2.5)
axis(2,las=2)
mtext(expression(paste("Initial CLR-midge")),side=1,line=2.5)
segments(inital.myco, cont.dif, inital.myco, ant.dif)


## now look at the the differences between the two treatments to see size of effect.
#### this includes both the positive and negative changes in Mycodiplosis numbers 
dif.props <- cont.dif-ant.dif
plot(dif.props,xlim=c(0,60),ylim=c(-1,1),xlab="",ylab="",yaxt="n")
mtext(expression(paste("Difference in prop ",Delta)),side=2,line=2.8)
mtext("(Cont - Treat)",side=2,line=2.2)
mtext(expression(paste("Initial CLR-midge")),side=1,line=2.5)
axis(2,las=2)
abline(h=0,col="red",lty=3)

## absolutely value plotted here to just look at the magnitude of the effect 

dif.props <- abs(cont.dif-ant.dif)
plot(dif.props,xlim=c(0,60),ylim=c(0,1.5),xlab="",ylab="",yaxt="n")
mtext(expression(paste("abs(Difference in prop ",Delta,")")),side=2,line=2.8)
mtext("(Cont - Treat)",side=2,line=2.2)
mtext(expression(paste("Initial CLR-midge")),side=1,line=2.5)
axis(2,las=2)
abline(h=0,col="red",lty=3)

### no signature of density dependence seen here. 
model <- lm(dif.props~ inital.myco)
abline(model,col="red")
summary(model)

