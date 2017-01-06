# DATA CHECKING AND SUMMARIES FOR THE BIODEV FIELD DATA
# Authors: J.Heiskanen, R.Valbuena & P.Packalen
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
#LOAD INPUT DATA 
##########################

# if changes were made to the Excel file, then Excel sheet should be saved as CSV-file and
# "1_ReadCSV.R" script run again

# load all .rda files in the working directory
input.datasets <- list.files(, pattern=".rda", full.names=TRUE)
for (i in 1:length(input.datasets)){
    load(file = input.datasets[i])
}
rm(i, input.datasets)


##########################
#PLOTS
##########################

# check that all clusters have 10 plots
plots.cluster <- aggregate(plots$plot, by=list(plots$cluster), length)
colnames(plots.cluster) <- c("Cluster", "No. plots")
print(plots.cluster)
rm(plots.cluster)

# plot positions, position errors, elevations and slopes
par(mfrow=c(2,2))
plot(plots$lon, plots$lat, main="Plot positions", xlab="Longitude (E)", ylab="Latitude (N)")
hist(plots$error, main="Estimated position error", xlab="Meters"); box()
hist(plots$elev, main="Elevation", xlab="Meters"); box()
hist(plots$slope, main="Slope", xlab="Degrees"); box()
# mean values
print(paste("Mean position error:", round(mean(plots$error, na.rm=TRUE),1)))
print(paste("Mean elevation:", round(mean(plots$elev),0)))
print(paste("Mean slope:", round(mean(plots$slope),0)))

# number of field measurement days
print(paste("Number of field measurement days:", length(unique(plots$date))))

# number of plots measured per field measurement day
plots.day <- aggregate(plots$date, by=list(plots$date), length)
summary(plots.day$x) #summary statistics
dev.off()
par(mfrow=c(1,2))
barplot(plots.day$x, xlab="Day", ylab="Number of plots"); box()
hist(plots.day$x, main="", xlab="Day", ylab="Number of plots"); box()
rm(plots.day)


##########################
#TREES (DBH > 10 CM)
##########################

# check IDs (are IDs within correct range?)
unique(sort(trees$cluster))
unique(sort(trees$plot))

# how many trees were measured in total?
print(paste("Number of measured trees (DBH > 10 cm):",length(trees$dbh)))

# how many plots have trees?
print(paste("Number of plots with trees (DBH > 10 cm):",length(unique(trees$plot.id))))

# summary for DBH
summary(trees$dbh)
dev.off()
hist(trees$dbh, main="DBH distribution", xlab="DBH"); box()
# are maximum values realistic?

# summaries for other variables
unique(trees$unusual) #should be only 0 or 1
unique(trees$dead)
print(paste("Number of unusual trees (DBH > 10 cm):",sum(trees$unusual)))
print(paste("Number of dead trees (DBH > 10 cm):",sum(trees$dead)))


##########################
#SAMPLE TREES
##########################

# check IDs (are IDs within correct range?)
unique(sort(s.trees$cluster))
unique(sort(s.trees$plot))

# how many sample trees were measured in total?
print(paste("Number of sample trees (DBH > 10 cm):",length(s.trees$dbh)))

# how many sample trees per plot?
# number of sample trees should be 3 if there are more than 3 trees in the plot
t <- aggregate(s.trees$dbh, by=list(s.trees$cluster, s.trees$plot), length)
names(t) <- c("cluster","plot","no.trees")
t
#plots that have more or less sample trees than 3
t[t$no.trees!=3,]

# summaries for DBH, H and CD
summary(s.trees$dbh)
summary(s.trees$h)
summary(s.trees$cd1)
summary(s.trees$cd2)

# plot DBH vs H and CD and look for errors
dev.off()
par(mfrow=c(2,2))
plot(s.trees$dbh, s.trees$h, xlab="DBH (cm)", ylab="H (m)" )
plot(s.trees$dbh, s.trees$cd1, xlab="DBH (cm)", ylab="Largest CD (m)" )
plot(s.trees$dbh, s.trees$cd2, xlab="DBH (cm)", ylab="Perpendicular CD (m)" )


##########################
#PALMS
##########################

#check IDs
unique(sort(palms$cluster))
unique(sort(palms$plot))
unique(sort(palms$palm))

#how many palms were measured in total?
print(paste("Number of palms (DBH > 10 cm):",length(palms$dbh)))

# how many plots have palms?
print(paste("Number of plots with palms (DBH > 10 cm):",length(unique(palms$plot.id))))

# summaries for DBH and H
summary(palms$dbh)
summary(palms$h)


##########################
#SMALL TREES (DBH 4-10 CM)
##########################

# check IDs
unique(sort(l.trees.4_10$cluster)) #1-16
unique(sort(l.trees.4_10$plot))    #1-10
unique(sort(l.trees.4_10$subplot)) #1-4

# summarize subplot counts and dbh
summary(l.trees.4_10$count)
summary(l.trees.4_10$dbh) #should be 4-10

# plot DBH vs H and CD
dev.off()
par(mfrow=c(2,2))
plot(l.trees.4_10$dbh, l.trees.4_10$h, xlab="DBH (cm)", ylab="H (m)" )
plot(l.trees.4_10$dbh, l.trees.4_10$cd1, xlab="DBH (cm)", ylab="Largest CD (m)" )
plot(l.trees.4_10$dbh, l.trees.4_10$cd2, xlab="DBH (cm)", ylab="Perpendicular CD (m)" )


##########################
#DEAD SMALL TREES (DBH 4-10 CM)
##########################

# check IDs
unique(sort(d.trees.4_10$cluster))
unique(sort(d.trees.4_10$plot))
unique(sort(d.trees.4_10$subplot))

# summarize subplot counts and dbh
summary(d.trees.4_10$count)
summary(d.trees.4_10$dbh) #should be 4-10


##########################
#DEAD WOOD LYING ON THE GROUND (DOWNED DEAD WOOD)
##########################

# check IDs
unique(sort(d.wood$cluster))
unique(sort(d.wood$plot))
unique(sort(d.wood$subplot))
unique(sort(d.wood$id))

# how many pieces of dead wood?
print(paste("Pieces of dead wood:",length(d.wood$d)))

# summarize length, diameter and decay class
summary(d.wood$length)
summary(d.wood$d)
unique(sort(d.wood$decay.class))

# number of pieces per decay class
t <- aggregate(d.wood$decay.class, by=list(d.wood$decay.class), length)
names(t) <- c("Decay class","Count")
t


##########################
#COMPUTE SPECIES FREQUENCIES
##########################

# trees DBH > 10 cm
species.count <- aggregate(trees$sp, by=list(trees$sp), length)
names(species.count) <- c("code","count")
species.count["freq"] <- round(species.count$count/(sum(species.count$count))*100,1)
species.count <- merge(species.count, species[,1:2], by="code", all=TRUE)
species.count <- species.count[order(-species.count$count),]
head(species.count)

# plot most common tree species (without "Unknown")
par(mfcol=c(1,1), mai=c(1.5,3,1,1))
sc.known <- subset(species.count, name!="Unknown")
barplot(sc.known$freq[1:15], names.arg=sc.known$name[1:15], horiz=T, 
        main="Most common species", xlab="Frequency (%)", las=1)

# frequency of the unknown species
round( sum( species.count[species.count$name=="Unknown","count"] ) / 
         sum(species.count[,"count"], na.rm=T) * 100 , 1)
# 6.7% of the trees could not be identified

# dominant species in subplots
dominant.species <- aggregate(l.trees.4_10$dominant.sp, 
                              by=list(l.trees.4_10$dominant.sp), length)
names(dominant.species) <- c("code","count")
dominant.species <- merge(dominant.species, species, by="code")
dominant.species <- dominant.species[order(-dominant.species$count),]
# combine
species.all <- merge(species.count, dominant.species, by="code", all=TRUE)
species.all <- species.all[,c(1,2,3,5,4)]
names(species.all) <- c("code","count","freq","dom.species","species")
#select only species that are in the data (if any left)
species.all <- subset(species.all, count!="NA" | dom.species!="NA")
# replace NAs by zero
species.all[is.na(species.all$count), "count"] <- 0
species.all[is.na(species.all$dom.species), "dom.species"] <- 0

# see results
head(species.all)

# write to file
# comment the following line if you do not want to write your results to a csv-file
write.csv(species.all, "../Results/SpeciesCountsFrequencies.csv", row.names=FALSE)


