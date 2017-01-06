# HEIGHT-DIAMETER MODELLING AND HEIGHT IMPUTATION
# Authors: R.Valbuena, J.Heiskanen & P.Packalen
###############################################################################


# Note: the script 1 should be run before running this script


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment and graphics
rm(list=ls()); graphics.off()

# specify path to your working directory
#setwd("C:/Temp/BIODEV/R/Data")


##########################
#LOAD PACKAGES
##########################

# the packages need to be installed from the internet
# run next line without commenting "#" if you use the package for the first time
#install.packages(pkgs="nlme", repos = "http://cran.r-project.org")

# also, the package needs to be loaded each time this sript is run
library(nlme)


##########################
#RUN FUNCTIONS
##########################

#####
# Curtis height function
#####

# Curtis function has independent variable (x), intercept (a) and slope (b)
# additional fixed-effects are added as "x2", "x3", etc.
# their coefficients modify the slope "b2", "b3", etc.
# they are zero by default, and therefore they do not exist unless specified

curtis <- function(x,a,b,	 # basic function
  x2=0,x3=0,x4=0,x5=0,	   # additional terms
  b2=0,b3=0,b4=0,b5=0) {   # coeffcients of additional terms
  1.3+(a*x)/((1+x)^(-b+b2*x2+b3*x3+b4*x4+b5*x5))
}

#####
# Functions for data fit
#####

# functions to print a plot showing model fit

# version for fixed-effects model
# prints observed values and line of prediction (fixed effects)
print.model.fit <- function(model,obs,x,y,main,col) {
	par(mfrow=c(1,1))
	obs.x <- obs[order(obs[,x]),]
	fit<-predict(model,obs.x)
	plot(obs.x[,c(x,y)],col=as.factor(obs.x[,col]),main=main)
	lines(obs.x[,x],fit,col="red",lty="dotted",lwd=2)
}

# version for mixed-effects model
# prints observed values and lines of prediction (mixed effects)
# the line of fixed-effects predicitons is depicted as well (dotted)
print.mx.model.fit <- function(model,obs,x,y,random,main,col=random) {
	par(mfrow=c(1,1))
	obs.x <- obs[order(obs[,x]),]
	fit.fix <- predict(model,obs.x,level=0)
	plot(obs.x[,c(x,y)],main=main)
	# mixed-effects
	plots <- unique(obs[,random])
	for (i in as.vector(plots)){
		subplot <- obs[obs[,random]==i,]
		subfit <- predict(model,subplot)
		x.seq <- seq(min(subplot[x])*.9,max(subplot[x])*1.1)
		df <- data.frame(dbh=x.seq,plot.id=rep(i,length(x.seq)))
		lines(x.seq,predict(model,df),col=i,lwd=1)
		points(subplot[,x],subplot[,y],col="white",lwd=1)
		points(subplot[,x],subplot[,y],col=i,lwd=1)
	}
	# fixed-effects
	lines(obs.x[,x],fit.fix,col="red",lty="dotted",lwd=2)
}

#####
# Function for accuracy assessment
#####

# prints RMSE and bias

print.accuracy<-function(model,obs,x,y,main) {
	# model fit
	obs.x <- obs[order(obs[,x]),]
	fit<-predict(model,obs.x,,level=1)
	# RMSE and bias
	print(main)
	res <- fit-obs.x[,y]
	bias <-signif(mean(res),4); cat("BIAS:",bias)
	bias.r <- signif(bias/mean(obs.x[,y])*100,4); cat( " metres (",bias.r,"%)\n",sep="")
	RMSE <- signif(sqrt(sum((res^2)/length(res))),4); cat("RMSE:",RMSE)
	RMSE.r <- signif(RMSE/mean(obs.x[,y])*100,4); cat( " metres (",RMSE.r,"%)\n ",sep="")
}

#####
# Function for testing assumptions
#####

# prints plots for diagnosis of model assumptions
# "mywhiskers" can be found in R package "lmfor"
# Author: L.Mehtätalo (UEF; Finland)

print.model.assumpt <- function(model,obs,x,col) {
	par(mfrow=c(2,2))
		mywhiskers<-function(x,y,nclass=10,limits=NA,add=FALSE,se=TRUE,main="",
                         xlab="x",ylab="y",ylim=NA,lwd=1) {
			away<-is.na(x+y)
			x<-x[!away]
			y<-y[!away]
			if (is.na(limits[1])) 
				limits<-seq(min(x),max(x)+1e-10,length=nclass+1) 
			else 
				nclass=length(limits)-1
			means<-sapply(1:nclass,function(i) mean(y[x>=limits[i]&x<limits[i+1]]))
			if (se) { 
				ses<-sapply(1:nclass,function(i) sd(y[x>=limits[i]&x<limits[i+1]])/
									sqrt(sum(x>=limits[i]&x<limits[i+1])))
			} else {
				ses<-sapply(1:nclass,function(i) sd(y[x>=limits[i]&x<limits[i+1]]))
			}
			lb<-means-1.96*ses
			ub<-means+1.96*ses
			xclass<-1/2*(limits[-1]+limits[-nclass-1])
			if (add) {
				points(xclass,means) 
			} else {
				if(is.na(ylim[1])) ylim<-c(min(lb),max(ub))
				plot(xclass,means,ylim=ylim,main=main,xlab=xlab,ylab=ylab,xlim=range(x))
			}
			sapply(1:nclass,function(i) lines(xclass[c(i,i)],c(lb[i],ub[i]),lwd=lwd))
			
		}
	
	####
	# Assumption 1: Model form
	####
	plot(obs[,x],resid(model),col=as.factor(obs[,col]),xlab=x,ylab="Residual",
       main="Model form")
	mywhiskers(obs[,x],resid(model),add=TRUE,lwd=2)
	mywhiskers(obs[,x],resid(model),se=FALSE,add=TRUE)
	abline(0,0)
	
	####
	# Assumption 2: Homocedasticity
	####
	plot(fitted(model),resid(model),col=as.factor(obs[,col]),main="Homocedasticity")
	mywhiskers(fitted(model),resid(model),add=TRUE,lwd=2)
	mywhiskers(fitted(model),resid(model),add=TRUE,se=FALSE)
	abline(0,0)
	
	####
	# Assumption 3: Normality of residuals
	####
	qqnorm(resid(model,type="pearson"),main="Normal QQ-plot of residuals")
	abline(0,1)
	hist(resid(model),main="Histogram of residuals")
}


##########################
#HEIGHT-DIAMETER MODELLING
##########################

##########################
#LOAD INPUT DATA 
##########################

# load all .rda files in the working directory
input.datasets <- list.files(,".rda")
for (i in 1:length(input.datasets)){
	load(file = input.datasets[i])
}
# remove unnecessary data and values for clarity
rm(i,input.datasets,d.wood,plots,palms,species)


##########################
#ADD SPECIES INFORMATION FOR SAMPLE TREES
##########################

# Species information is currently lacking from the s.trees data frame (i.e. sample trees 
# with both DBH and H measured). However, species is available in trees data frame. 
# Therefore, we join this information to the sample trees.

s.trees <- merge(s.trees, trees[,c("plot.id","tree","sp")], 
                 by=c("plot.id", "tree"))


##########################
#NON-LINEAR FIXED-EFFECTS MODEL
##########################

# fit the model
fm1 <- nls(h~curtis(dbh,a,b),
		   data=s.trees,
		   start=list(a=1,b=-.4)
		   )

# model estimates:
summary(fm1)
# those are all significant (***)

# model fit:
graphics.off()
print.model.fit(fm1,s.trees,"dbh","h",col="sp",
                main="NON-LINEAR FIXED-EFFECTS MODEL")
# circles denote the sample observations
# colours denote different species
# the red dotted line is the fixed-effect model fit
# (i.e. predictions are carried out along this line)

# accuracy:
print.accuracy(fm1,s.trees,"dbh","h", main="NON-LINEAR FIXED-EFFECTS MODEL")
# the model seems to fit fairly well
# BIAS: 0.01806 metres (0.1697%)
# RMSE: 2.102 metres (19.75%)
# the model is unbiased but RMSE is a bit high

# model assumptions:
print.model.assumpt(fm1,s.trees,"dbh",col="sp") 
# circles denote residuals and colours different species

# assumption 1: model form
# no trend can be observed in the residuals	

# assumption 2: homocedasticity
# equal dispersion along the range

# assumption 3: normality of residuals
# a bit skewed, but shall be assumed


##########################
#NON-LINEAR MIXED-EFFECTS MODELS
##########################

##########################
#ADDING PLOT AS RANDOM EFFECTS
##########################

# fitting the mixed-efects model
nlme2 <- nlme(h~curtis(dbh,a,b),
		     fixed=list(a~1,b~1),
		     random=b~1|plot.id,	# random effects affect the slope of the curve only
		     data=s.trees,
		     start=c(a=round(coef(fm1)[1],2),b=round(coef(fm1)[2],2))
		     )

# model estimates:
summary(nlme2)
# those are all significant (p-value = 0)

# model fit:
# each plot is calibrated according to 1-3 observations
# these observation increase/decrease the slope of the model for each plot
graphics.off()
print.mx.model.fit(nlme2,s.trees,"dbh","h","plot.id",
                   main="MIXED-EFFECTS MODEL (RANDOM = PLOT)")
# circles denote the sample observations
# the red dotted line is the fixed-effect model fit
# thin lines and different colours denote different plots

# accuracy:
print.accuracy(nlme2,s.trees,"dbh","h",
               main="MIXED-EFFECTS MODEL (RANDOM = PLOT)")
# the model seems to fit fairly well
# BIAS: 0.06958 metres (0.6537%)
# RMSE: 1.43 metres (13.43%)
# the model is unbiased and RMSE has decreased in comparison to the fixed-effect model

# model assumptions:
print.model.assumpt(nlme2,s.trees,"dbh",col="sp")
# circles denote residuals and colours different species

# assumption 1: model form
# no trend can be observed in the residuals	

# assumption 2: homocedasticity
# equal dispersion along the range

# assumption 3: normality of residuals
# normally distributed


##########################
#APPLYING THE MODEL: HEIGHT IMPUTATION
##########################

# finally, we will impute missing heights
# in other words, best possible height estimate is predicted for those trees that are
# missing height information
# below, extra column is added for each model

####
# use measured height when available
####

# merge ("join") trees and s.trees
trees <- merge(trees, s.trees[,c("cluster","plot","tree","h")], 
                 by=c("cluster","plot","tree"), all.x=TRUE)

####
# fixed-effects model
####

trees["h.fm1"] <- round(predict(fm1, trees),2)

# for those trees whose height was measured, the prediction is substituted 
# by the measured value
trees$h.fm1[is.na(trees$h)==F] <- trees$h[is.na(trees$h)==F]

####
# random-effects = plot
####

# use mixed-effects model for all plots that have height measurements
trees[trees$plot.id%in%s.trees$plot.id==T,"h.nlme2"] <- 
  round(predict(nlme2,trees[trees$plot.id%in%s.trees$plot.id==T,]),2)

# the plots without height measurements cannot be calibrated for the random effects
# for those plots, take the fixed effects prediction
trees$h.nlme2[is.na(trees$h.nlme2)==T] <- trees$h.fm1[is.na(trees$h.nlme2)==T]

# for those trees whose height was measured, the prediction is substituted
# by the measured value
trees$h.nlme2[is.na(trees$h)==F] <- trees$h[is.na(trees$h)==F]

# see column names and summarize the height values
names(trees)
summary(trees[,c("h.fm1","h.nlme2")])


##########################
# IMPUTE HEIGHTS FOR DEAD TREES DBH = 4-10 CM
##########################

# Tree heights were not measured for "Standing dead trees DBH 4-10 cm"
# but those were measured for "Living trees DBH 4-10 cm". Below, 
# we fit a linear model using living trees and predict missing heights for dead trees
# based on that model.

head(d.trees.4_10) #h missing
head(l.trees.4_10) #h measured

# plot observations
graphics.off()
plot(l.trees.4_10$dbh,l.trees.4_10$h, xlab="dbh", ylab="h",
     xlim=c(0,10),ylim=c(1.3,13),
     col=as.factor(l.trees.4_10$dominant.sp))

# fit linear model with intercept fixed to 1.3 m i.e. breast height
fnct <- function(x,b) {1.3+(b*x)} 
fmd <- nls(h ~ fnct(dbh,b),
           data=l.trees.4_10,
           start=list(b=.5))
summary(fmd)
lines(c(0,l.trees.4_10$dbh), c(1.3,fitted(fmd)))

graphics.off()

# predict missing heights
d.trees.4_10["h"] <- round(predict(fmd, d.trees.4_10), 2)
summary(d.trees.4_10$h)


##########################
#UPDATE RDA-FILES
##########################

# update existing rda files
save(trees, file = "Sanya_input_trees.rda")
save(d.trees.4_10, file = "Sanya_input_snags.rda")


##########################
#WRITE A FILE WITH IMPUTED HEIGHTS
##########################

# comment the following line if you do not want to write your results to a csv-file
write.csv(trees,"../Results/TreesWithHeights.csv", row.names=FALSE)


