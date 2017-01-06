# ESTIMATE MEANS AND TOTALS BASED ON FIELD DATA
# Author: J.Heiskanen
###############################################################################


# Estimate means and totals with standard errors (SE) and 95% confidence intervals
# Computations assume simple random sampling without replacement (Kangas, 2006)

# Note: Some plots fall outside 10 km x 10 km sentinel site. However, those plots
# are not removed from calculations in this script.

# Note: the scripts 1, 3, 4, 5 and 6 should be run before running this script


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment and graphics
rm(list=ls()); graphics.off()

# specify path to your working directory
#setwd("C:/Temp/BIODEV/R/Data")


##########################
#RUN FUNCTIONS
##########################

#####
# Function to compute mean and total
#####

meanTotal <- function(variable) {
  
  # total area of the site (10,000 ha)
  A <- 10000
  # sampling units in the population
  # plot size = 0.1 ha
  N <- 10000/0.1
  # number of the sample plots
  n <- length(variable)
  
  # sample mean
  mean <- 1/n*sum(variable)
  
  # variance of the mean
  var.mean <- (1-(n/N))*(var(variable)/n)
  
  # standard error of the mean
  se.mean <- sqrt(var.mean)
  
  # 95% confidence interval of the mean
  ci95.min <- round(mean-(1.96*se.mean),2)
  ci95.max <- round(mean+(1.96*se.mean),2)
  
  # total
  total <- A*mean
  
  # standard error of the total
  se.total <- sqrt(A^2*var.mean)
  
  # print results
  cat("Mean:", round(mean,1), "\n")
  cat("SE of the mean:", round(se.mean,1), "\n")
  cat("95% CI of the mean:", ci95.min,"-",ci95.max, "\n")
  cat("Total:", round(total,2), "\n")
  cat("SE of total:", round(se.total,2), "\n\n")
  
  # values
  values <- as.vector( c(round(mean,3), round(se.mean,3), round(ci95.min,3),
                        round(ci95.max,3), round(total,3), round(se.total,3)) )
  
}


##########################
#LOAD INPUT DATA 
##########################

#load plot-level results
load("PlotData_Sanya.rda")    


##########################
#COMPUTE MEANS AND TOTALS
##########################

#check variable names
names(Plot.data)

####
# single attributes
####

# aboveground carbon
meanTotal(Plot.data$agc.ha)
# belowground carbon (roots)
meanTotal(Plot.data$bgc.ha) 
# deadwood carbon (standing and downed)
meanTotal(Plot.data$agc.ha.sdt + Plot.data$agc.ha.ddw) 

####
# several attributes to table
####

# add dead wood column
Plot.data["dead.wood"] <- Plot.data$agc.ha.sdt + Plot.data$agc.ha.ddw

# add here al the attributes you want
# for some attributes only mean is relevant (e.g. mean height)
attr <- c("agc.ha","bgc.ha","dead.wood","v.ha")

# compute mean and total
res <- c()
for (i in attr) {
  res <- rbind(res, meanTotal(Plot.data[,i]))
}
res <- cbind(attr, res)
res <- as.data.frame(res)
names(res) <- c("Attribute","Mean","SE.mean","CI.min","CI.max","Total","SE.total")
res


##########################
#WRITE RESULTS TO A FILE
##########################

write.csv(res, "../Results/Results.csv", row.names=FALSE)


# clean the working environment and graphics
rm(list=ls()); graphics.off()
