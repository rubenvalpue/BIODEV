# COMPUTE PLOT-LEVEL STATISTICS
# Authors: J.Heiskanen, R.Valbuena & P.Packalen
###############################################################################


# Note: the scripts 1, 3 and 4 should be run before running this script


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
rm(d.trees.4_10, d.wood, l.trees.4_10, palms, s.trees, species, trees)
rm(Plot.data) #if this script has been run before


##########################
#COMPUTE STEM COUNT, BASAL AREA, AGB/AGC, BGB/BGC AND VOLUME PER HA
##########################

# count.ha = stems/ha
# ba.ha = m^2/ha
# agb.ha = Mg/ha (Mg = 1000 kg)
# bgb.ha = Mg/ha
# agc.ha = Mg/ha
# bgc.ha = Mg/ha
# v.ha = m^3/ha

# Multiplying a measurement by Hectare Expansion Factor (HEF) provides directly
# its absolute contribution to the per-hectare total. 

# DBH in cm is divided by 100 to get basal area in m^2
TreeList$ba.ha <- (pi*(TreeList$dbh/2/100)^2) * TreeList$HEF

# AGB, BGB, AGC and BGC are divided by 1000 in order to convert kg to Mg
TreeList$agb.ha <- TreeList$agb * TreeList$HEF / 1000
TreeList$bgb.ha <- TreeList$bgb * TreeList$HEF / 1000
TreeList$agc.ha <- TreeList$agc * TreeList$HEF / 1000
TreeList$bgc.ha <- TreeList$bgc * TreeList$HEF / 1000

# volume is already in m^2
TreeList$v.ha <- TreeList$v * TreeList$HEF

# compute attributes separately for each "tree.type" ("living", "SDT" or "DDW")
Plot.types <- c()
Plot.types[c("plot.id","tree.type","count.ha","ba.ha","agb.ha","bgb.ha",
             "agc.ha","bgc.ha","v.ha")] <-
  aggregate(TreeList[,c("HEF","ba.ha","agb.ha","bgb.ha","agc.ha","bgc.ha","v.ha")],
            by=list(TreeList$plot.id,TreeList$tree.type), sum)
Plot.types <- as.data.frame(Plot.types)


##########################
#COMPUTE MEAN DBH AND TREE HEIGHT
##########################

# dbh.mean = cm
# h = m
# subset only living trees
lt <- TreeList[TreeList$tree.type=="living",]

# mean DBH is computed as DBH weighted by HEF
dbh.mean <- sapply(split(lt, lt$plot.id),
                   function(x) weighted.mean(x$dbh,x$HEF))

# quadratic mean DBH (i.e. diameter that corresponds to the mean basal area)
# is computed as DBH weighted by basal area
dbh.qdr.mean <- sapply(split(lt, lt$plot.id),
                       function(x) weighted.mean(x$dbh,x$ba.ha))

# arithmetic mean height is computed as height weighted by HEF
h.mean <- sapply(split(lt, lt$plot.id),
                 function(x) weighted.mean(x$height,x$HEF))

# Lorey's mean height is computed as height weighted by basal area
h.loreys <- sapply(split(lt ,lt$plot.id),
                   function(x) weighted.mean(x$height,x$ba.ha))

Plot.mean <- cbind(dbh.mean, dbh.qdr.mean, h.mean, h.loreys)
Plot.mean <- as.data.frame(Plot.mean)
Plot.mean$plot.id <- row.names(Plot.mean)

rm(lt, dbh.mean, dbh.qdr.mean, h.mean, h.loreys)


##########################
#ORGANIZE TO SINGLE DATAFRAME
##########################

# merge everything to new dataframe
# living trees
Plot.data <- merge(plots[,c("plot.id","cluster","plot")], 
                   Plot.types[Plot.types$tree.type=="living", c(-2)],
                   by="plot.id", all.x=TRUE)
# add mean dbh and height for living trees
Plot.data <- merge(Plot.data, Plot.mean, by="plot.id", all.x=TRUE)
# add dead standing trees
Plot.data <- merge(Plot.data, 
                   Plot.types[Plot.types$tree.type=="SDT", c(-2)],
                   by="plot.id", all.x=TRUE,
                   suffixes = c("",".sdt"))
# add downed dead wood (basal area is not relevant)
Plot.data <- merge(Plot.data,
                   Plot.types[Plot.types$tree.type=="DDW",
                              c("plot.id","count.ha","agb.ha","v.ha","agc.ha")],
                   by="plot.id", all.x=TRUE,
                   suffixes = c("",".ddw"))

Plot.data[is.na(Plot.data)] <- 0 #change NAs to zero

names(Plot.data)
# suffix "sdt" refers to standing dead trees
# suffix "ddw" refers to downed dead wood

rm(Plot.mean, Plot.types)


##########################
#EXPLORE PLOT DATA
##########################

# check variable names
names(Plot.data)

par(mfrow=c(3,2),mar=c(4,4,3,2)+0.1)
hist(Plot.data$count.ha, main="Number of stems", 
     xlab=expression(paste("stems ",ha^-1)), col="grey"); box()
hist(Plot.data$ba.ha, main="Basal area",
     xlab=expression(paste(m^2," ",ha^-1)), col="grey"); box()
hist(Plot.data$dbh.qdr.mean, main="Mean DBH", xlab="cm", col="grey"); box()
hist(Plot.data$h.loreys, main="Mean height", xlab="m", col="grey"); box()
hist(Plot.data$agc.ha, main="Aboveground carbon",
     xlab=expression(paste("Mg C ",ha^-1)), col="grey"); box()
hist(Plot.data$agc.ha.sdt+Plot.data$agc.ha.ddw, 
     main="Dead wood carbon",
     xlab=expression(paste("Mg C ",ha^-1)), col="grey"); box()


##########################
#SAVE RESULTS AS RDA-FILE
##########################

save(Plot.data, file = "PlotData_Sanya.rda")


##########################
#WRITE A FILE WITH PLOT-LEVEL RESULTS
##########################

# comment the following line if you do not want to write your results to a csv-file
write.csv(Plot.data, file="../Results/PlotData.csv", row.names=FALSE)


