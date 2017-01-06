# COMPUTE CANOPY COVER
# Author: J.Heiskanen
###############################################################################


# Note: the scripts 1, 3, 4 and 5 should be run before running this script


# Following terminology of Nilson and Kuusk (2004).
# Canopy cover (c.can) is the proportion of ground surface covered by tree crown 
# projections, the overlapping parts of the crowns counted only once
# Crown cover (c.cr) is the coverage of ground surface by tree crown projections, 
# the overlapping parts of the crowns counted as many times as crown are overlapping.
#
# Crowns are non-transparent.
# c.can is in the range of 0-1 (0-100%).
#
# c.can and c.cr are related as follows:
# c.can = 1-exp(-c*c.cr)
# where c is a parameter that indicates the deviation of the tree positions 
# from the Poisson distribution (Nilson and Kuusk, 2004).
#
# c = 1, tree distribution is random (Poisson)
# c < 1, tree distribution is clustered
# c > 1, tree distribution is regular
#
# Additional assumptions:
# Crowns are assumed circles; diameter corresponds to the mean of two perpendicular 
# measurements. Only living trees are considered.
# If tree has multiple stems, only stem with the largest crown diameter is included


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment and graphics
rm(list=ls()); graphics.off()

# specify path to your working directory
#setwd("C:/Temp/BIODEV/R/Data")


##########################
#LOAD INPUT DATA 
##########################

# load all .rda files in the working directory
input.datasets <- list.files(, pattern=".rda", full.names=TRUE)
for (i in 1:length(input.datasets)){
  load(file = input.datasets[i])
}
# remove unncessary data and values
rm(i, input.datasets)


##########################
#PREDICT MISSING CROWN DIAMETERS (CD)
##########################

# merge trees and sample trees
# remove unusual and dead trees
# lt = "large trees"
lt <- merge( trees[trees$unusual==0 & trees$dead==0, 
                   c("plot.id","tree","dbh","unusual","dead")],
             s.trees[c("plot.id","tree","cd1","cd2")],
             by=c("plot.id","tree"), all.x=TRUE )

# compute mean CD
lt["cd"] <- (lt$cd1+lt$cd2)/2

# explore data
summary(lt$cd)
boxplot(lt$cd, ylab="Mean crown diameter (m)")
xdim <- ceiling(max(lt$dbh)) + 2;
plot(lt$dbh, lt$cd, 
     xlim=c(0,xdim), xlab="DBH (cm)", ylab="Crown diameter (m)")

# fit linear regression model using 'lm'
m <- lm(cd ~ dbh, lt)
summary(m)

# plot fitted model
par(mfrow=c(1,2))
xdim <- ceiling(max(lt$dbh)) + 2;
plot(lt$dbh, lt$cd, xlim=c(0,xdim), xlab="DBH (cm)", 
     ylab="Crown diameter (m)")
abline(m)

# plot residuals
plot(m$fitted.values, m$residuals, xlab="Fitted values", ylab="Residuals")
abline(0,0)
# is linear model appropriate?

# impute cd for all trees
lt[is.na(lt$cd), "cd"] <- predict(m, lt[is.na(lt$cd),])

# remove unnecessary columns
lt <- lt[,c("plot.id","tree","cd")]
summary(lt$cd)

rm(m, xdim)


##########################
#REMOVE MULTIPLE TRUNKS
##########################

# remove trunk information by removing letters from tree IDs
lt["tree"] <- as.numeric(gsub("[^0-9]", "", lt[,"tree"]))

# aggregate to tree-level
lt <- aggregate(lt$cd, 
                by=list(lt$plot.id, lt$tree), max)
names(lt) <- c("plot.id","tree","cd")


##########################
#COMPUTE CROWN COVER (C.CR) OF LARGE TREES
##########################

# crown areas of the single trees
lt["ca"]  <- (pi*lt$cd^2)/4

lt.p <- aggregate(lt$ca, by=list(lt$plot.id), sum)
names(lt.p) <- c("plot.id","ca")

# merge with plots to get radius
lt.p <- merge(plots[c("plot.id","radius")], lt.p, by=c("plot.id"), all.x=TRUE)
lt.p[is.na(lt.p$ca), "ca"] <- 0

# compute crown cover
lt.p["c.cr"] <- lt.p$ca/(pi*lt.p$radius^2)*100
lt.p <- lt.p[,c("plot.id","c.cr")]
summary(lt.p$c.cr)


##########################
#COMPUTE C.CR FOR TREES DBH 4-10 CM
##########################

st <- l.trees.4_10[c("plot.id","count","cd1","cd2")]

# cd and ca for each tree
st["cd"] <- (st$cd1+st$cd2)/2
st["ca"] <- (pi*st$cd^2)/4

# ca and c.cr for each plot
st.p <- aggregate(st$ca, by=list(st$plot.id), sum)
names(st.p) <- c("plot.id","ca")
st.p["ca"] <- st.p$ca #total ca per 4 x 0.01 ha plots
st.p["c.cr"] <- st.p$ca/400*100
st.p <- st.p[,c("plot.id","c.cr")]
summary(st.p$c.cr)


##########################
#MERGE LARGE TREES AND SMALL TREES
##########################

# large and small trees
cc.p <- merge(lt.p, st.p, by=c("plot.id"), all.x=TRUE)
names(cc.p) <- c("plot.id","lt.c.cr","st.c.cr")
cc.p[is.na(cc.p$st.c.cr), "st.c.cr"] <- 0


##########################
#COMPUTE TOTAL C.CR
##########################

# no palms in the data
cc.p["c.cr"] <- cc.p$lt.c.cr + cc.p$st.c.cr

# summarize total c.cr
summary(cc.p$c.cr)


##########################
#COMPUTE CANOPY COVER (C.CAN)
##########################

# set value for c-parameter (see above for explanation)
c <- 1 #Poisson distributed stems

cc.p["lt.c.can"] <- ( 1-exp(-c*(cc.p$lt.c.cr)/100) )*100 #large trees only
cc.p["st.c.can"] <- ( 1-exp(-c*(cc.p$st.c.cr)/100) )*100 #small trees only
cc.p["c.can"] <- ( 1-exp(-c*(cc.p$c.cr)/100) )*100 #all trees
summary(cc.p$lt.c.can)
summary(cc.p$st.c.can)
summary(cc.p$c.can)

graphics.off()
plot(cc.p$c.cr, cc.p$c.can, xlab="Crown cover (%)", ylab="Canopy cover (%)")


##########################
#UPDATE RDA-FILE
##########################

# add canopy cover (c.can) to "Plot.data"

Plot.data$c.can <- NULL #remove c.can if already exist

Plot.data <- merge(Plot.data, cc.p[c("plot.id","c.can")],
                   by=c("plot.id"), all.x=TRUE )
# histogram
hist(Plot.data$c.can, main="Canopy cover", xlab="%", col="grey"); box()

save(Plot.data, file = "PlotData_Kou.rda")


##########################
#UPDATE FILE WITH PLOT-LEVEL RESULTS
##########################

# comment the following line if you do not want to write your results to a csv-file
write.csv(Plot.data, file="../Results/PlotData.csv", row.names=FALSE)


# clean the working environment and graphics
rm(list=ls()); graphics.off()
