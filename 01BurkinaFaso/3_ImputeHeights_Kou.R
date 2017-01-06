# HEIGHT-DIAMETER MODELLING AND HEIGHT IMPUTATION
# Authors: R.Valbuena, J.Heiskanen & P.Packalen
###############################################################################


# Note: the script 1 should be run before running this script


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment
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

# version for mixed-effects model with additional (fixed) additive terms 
# prints observed values (circles) and
# lines of fixed-effects prediction + final predictions (mixed effects / triangles)
print.mx2.model.fit<-function(model,obs,x1,x2,y,random,main,col=x2) {
  par(mfrow=c(1,1))
  obs.x <- obs[order(obs[,x1]),]
  fit.fix<-predict(model,obs.x,level=0)
  fit.ran<-predict(model,obs.x,level=1)
  # fixed-effects
  df<- cbind(obs.x,fit.fix)
  plot(obs.x[,c(x1,y)],main=main)
  colors<-df[,col]
  for (i in x2) {
    categories <- unique(df[,i])
    for (j in categories){ if (j!=0){
      subplot0 <- df[df[,i]==j,]
      lines(subplot0[,c(x1,"fit.fix")],col=subplot0[,col])
    }}
  }	
  # fixed-effects
  df<- cbind(obs.x,fit.ran)
  points(df[,c(x1,"fit.ran")],col=colors,pch=2,lwd=2)
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
  plot(obs[,x],resid(model),col=as.factor(obs[,col]),xlab=x,ylab="residual",
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
rm(i,input.datasets,d.wood,plots,species)


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
# BIAS: -8.77e-05 metres (-0.001112%)
# RMSE: 2.022 metres (25.64%)
# the model is unbiased but RMSE is a bit high

#model assumptions:
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
nlme2<-nlme(h~curtis(dbh,a,b),
		fixed=list(a~1,b~1),
		random=b~1|plot.id,	# random effects affect the slope of the curve only
		data=s.trees,
		start=c(a=round(coef(fm1)[1],2),b=round(coef(fm1)[2],2))
)

# model estimates:
summary(nlme2)
# those are all significant

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
# BIAS: 0.07992 metres (1.014%)
# RMSE: 1.457 metres (18.48%)
# the model is unbiased and RMSE has decreased in comparison to the fixed-effect model

#model assumptions:
print.model.assumpt(nlme2,s.trees,"dbh",col="sp")
# circles denote residuals and colours different species

# assumption 1: model form
# no trend can be observed in the residuals	

# assumption 2: homocedasticity
# equal dispersion along the range

# assumption 3: normality of residuals
# normally distributed

# from the colors it can be observed that residuals may depend on species
# next, we will try to use species as fixed-effects


##########################
#ADDING SPECIES AS FIXED EFFECTS
##########################

# fitting the mixed-efects model
nlme3<-nlme(h~curtis(dbh,a,b),
            fixed=list(a~1,b~as.factor(sp)),
            random=b~1|plot.id,
            data=s.trees,
            start=c(round(coef(fm1)[1],2),rep(round(coef(fm1)[2],2),
                                              nlevels(as.factor(s.trees$sp)))),
)

# model estimates:
summary(nlme3)
# not all the species have significant effect, but just a few

# model fit:
print.mx2.model.fit(nlme3,s.trees,"dbh","sp","h","plot.id",
                    main="MIXED-EFFECTS MODEL (FIXED = SPECIES + RANDOM = PLOT)")
# circles denote the sample observations
# colours denote different species
# the coloured lines are the fixed-effect models (one line per species)
# triangles denote predictions. However, predictions are not carried out along each line, 
# because they also depend on the random (plot) effects which are not shown in this plot.

# accuracy:
print.accuracy(nlme3,s.trees,"dbh","h",
               main="MIXED-EFFECTS MODEL (FIXED = SPECIES + RANDOM = PLOT)")
# the model seems to fit fairly well
# BIAS: 0.04916 metres (0.6235%)
# RMSE: 1.147 metres (14.55%)
# the model is unbiased and RMSE has decreased but as model estimates are non-significant,
# it may actually be not true

print.model.assumpt(nlme3,s.trees,"dbh",col="sp")
# circles denote residuals
# colours denote different species

# assumption 1: model form
# no trend can be observed in the residuals	

# assumption 2: homocedasticity
# equal dispersion along the range

# assumption 3: normality of residuals
# quite normally distributed
# dependence of species has been removed

# However, the effect of the most species was non-significant
# model nlme3 is therefore not valid
# Next, we will select only the most significant species


##########################
#ADDING ONLY SIGNIFICANT SPECIES AS FIXED EFFECTS
##########################

####
# add dummy variables
####

# only species significant at a 99% CI were:
#	number 8 (Anogeissus leiocarpus);
#	number 13 (Burkea africana);
#	number 57 (Combretum molle).

# It is of particular interest to have a specific field (dummy variable) 
# denoting which tree belongs to those species

# Anogeissus leiocarpus.
# Adding it to sample tree H-DBH data (for modelling)
s.trees["Anogeissus.leiocarpus"] <- 0
s.trees[s.trees$sp==8, "Anogeissus.leiocarpus"] <- 1
# Adding it to rest of tree DBH data (for imputation)
trees["Anogeissus.leiocarpus"] <- 0
trees[s.trees$sp==8, "Anogeissus.leiocarpus"] <- 1

# Burkea africana.
s.trees["Burkea.africana"] <- 0
s.trees[s.trees$sp==13, "Burkea.africana"] <- 1
trees["Burkea.africana"] <- 0
trees[s.trees$sp==13, "Burkea.africana"] <- 1

# Combretum molle.
s.trees["Combretum.molle"] <- 0
s.trees[s.trees$sp==57, "Combretum.molle"] <- 1
trees["Combretum.molle"] <- 0
trees[s.trees$sp==57, "Combretum.molle"] <- 1

# Remaining species.
# Adding it to sample tree H-DBH data (for modelling)
s.trees["Rem"] <- 1
s.trees[s.trees$sp==8, "Rem"] <- 0
s.trees[s.trees$sp==13, "Rem"] <- 0
s.trees[s.trees$sp==57, "Rem"] <- 0
# Adding it to rest of tree DBH data (for imputation)
trees["Rem"] <- 1
trees[trees$sp==8, "Rem"] <- 0
trees[trees$sp==13, "Rem"] <- 0
trees[trees$sp==57, "Rem"] <- 0

# the importance of these species can be observed 
# in the plot of residuals for the mixed-effects model (nmle2):
plot(predict(nlme2),resid(nlme2),col=as.factor(s.trees$Anogeissus.leiocarpus),
     pch=s.trees$Anogeissus.leiocarpus+1, main="Anogeissus leiocarpus")
abline(0,0,col="red")
plot(predict(nlme2),resid(nlme2),col=as.factor(s.trees$Burkea.africana),
     pch=s.trees$Burkea.africana+1, main="Burkea africana")
abline(0,0,col="red")
plot(predict(nlme2),resid(nlme2),col=as.factor(s.trees$Combretum.molle),
     pch=s.trees$Combretum.molle+1, main="Combretum molle")
abline(0,0,col="red")
# red triangles denote residuals for target species
# circles denote rest of the residuals

####
# fit model
####

# the fixed-effects model is only run for the purpose of computing 
# an initial guess for the estimates
fm4<-nls(h~curtis(dbh,a,b,
                Anogeissus.leiocarpus,Burkea.africana,Combretum.molle,
                b2=b2,b3=b3,b4=b4),	data=s.trees,start=list(a=1,b=-.4,b2=-.1,b3=-.01,b4=-.01))
# ...which is used in to compute the mixed-effects model
nlme5<-nlme(h~curtis(dbh,a,b,
                   Anogeissus.leiocarpus,Burkea.africana,Combretum.molle,
                   b2=b2,b3=b3,b4=b4),
            fixed=list(a~1,b~1,b2~1,b3~1,b4~1),
            random=b~1|plot.id,
            data=s.trees,
            start=c(round(coef(fm4)[1],2),round(coef(fm4)[2],2),
                    round(coef(fm4)[3],2),round(coef(fm4)[4],2),round(coef(fm4)[5],2)),
)

# model estimates:
summary(nlme5)
# they are all significant

# model fit:
print.mx2.model.fit(nlme5,s.trees,"dbh",
                    c("Anogeissus.leiocarpus","Burkea.africana","Combretum.molle","Rem"),
                    "h","plot.id", col="sp",
                    main="MIXED-EFFECTS (FIXED = SIG. SPECIES + RANDOM = PLOT)")
# circles denote the sample observations
# colours denote different species
# the coloured lines are the fixed-effect models
#	grey = Anogeissus leiocarpus
#	blue = Burkea africana 
#	black = Combretum molle 
#	magenta = rest of species
# triangles denote predictions (each colour is a different species)
# however, predictions are not carried out along each line, 
# because they also depend on the random (plot) effects which are not shown in this plot

# accuracy:
print.accuracy(nlme5,s.trees,"dbh","h",
               main="MIXED-EFFECTS MODEL (FIXED = signf.SPECIES + RANDOM = PLOT)")
# the model seems to fit fairly well
# BIAS: 0.05924 metres (0.7513%)
# RMSE: 1.283 metres (16.27%)
# model is unbiased and RMSE has been improved in respect to nmle2
# because all effects are significant, this is a valid model

# model assumptions:
print.model.assumpt(nlme5,s.trees,"dbh",col="sp")
# circles denote residuals
# colours denote different species

# assumption 1: model form
# no trend can be observed in the residuals	

# assumption 2: homocedasticity
# equal dispersion along the range

# assumption 3: normality of residuals
# the model is really good in terms of normality
# dependence of species has been removed by this model as well


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

####
# fixed-effects = significant species + random-effects = plot
####

# use mixed-effects model for all plots that have height measurements
trees[trees$plot.id%in%s.trees$plot.id==T,"h.nlme5"] <- 
  round(predict(nlme5, trees[trees$plot.id%in%s.trees$plot.id==T,]),2)

# the plots without height measurements cannot be calibrated for the random effects
# for those plots, take the fixed effects prediction
trees$h.nlme5[is.na(trees$h.nlme5)==T] <- trees$h.fm1[is.na(trees$h.nlme5)==T]

# for those trees whose height was measured, the prediction is substituted
# by the measured value
trees$h.nlme5[is.na(trees$h)==F] <- trees$h[is.na(trees$h)==F]

# see column names and summarize the height values
names(trees)
summary(trees[,c("h.fm1","h.nlme2","h.nlme5")])


##########################
#IMPUTE HEIGHTS FOR DEAD TREES DBH = 4-10 CM
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

# update existing rda file
save(trees, file = "Kou_input_trees.rda")
save(d.trees.4_10, file = "Kou_input_snags.rda")


##########################
#WRITE A FILE WITH IMPUTED HEIGHTS
##########################

# comment the following line if you do not want to write your results to a csv-file
write.csv(trees,"../Results/TreesWithHeights.csv", row.names=FALSE)

