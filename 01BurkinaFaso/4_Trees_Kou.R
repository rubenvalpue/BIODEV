# MAKE TREELIST WITH ABOVEGROUND AND BELOWGROUND BIOMASS/CARBON AND VOLUME
# Authors: J.Heiskanen, R.Valbuena & P.Packalen
###############################################################################


# Note: the scripts 1 and 3 should be run before running this script


##########################
#SET WORKING DIRECTORY AND RUN SCRIPTS
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

# remove unncessary dataframes and values
rm(i, input.datasets)
rm(TreeList) #if this script has been run before


##########################
#SET CONSTANTS
##########################

# wood density for dead wood
d.wd <- mean(species$wd.mean) #mean value for species in Kou

# decay class reduction factors (Waddell 2002)
rf.1 <- 1.0  #sound
rf.2 <- 0.78 #intermediate
rf.3 <- 0.45 #rotten

# root:shoot ratio (Mokany et al. 2006): median root:shoot ratio for 
# "Tropical/subtropical dry forest/plantation, shoot biomass > 20 Mg/ha"
rs.mokany <- 0.275

# carbon fraction of aboveground forest biomass (IPCC 2006)
c.f <- 0.47


###########################
#ADD WOOD DENSITY FOR TREES
###########################

# trees, DBH > 10 cm
trees <- merge(trees, species[,c("code","wd.mean")], by.x="sp", by.y="code",
               all.x=TRUE)

# trees, DBH 4-10 cm
l.trees.4_10 <- merge(l.trees.4_10, species[,c("code","wd.mean")], 
                      by.x="dominant.sp", by.y="code", all.x=TRUE)


##########################
#ALLOMETRIC MODELS FOR ABOVEGROUND BIOMASS
##########################

#####
# trees
#####

# Chave et al. (2014: equation 4)
# recommended model when tree height (H) has been measured.
chave14.4 <- function(dbh,h,wd) {
  0.0673*( (wd*(dbh^2)*h)^0.976 )}

# Chave et al. (2014: equation 7)
# recommended model if tree height was not measured.
chave14.7 <- function(dbh,wd,E) {
  exp( -1.803 - 0.976*E + 0.976*log(wd) + 2.673*log(dbh) - 0.0299*log(dbh)^2 )}
# this model requires environmental stress parameter (E)
# for details, see: http://chave.ups-tlse.fr/pantropical_allometry.htm
# for Kou, Burkina Faso, E=0.701229
E <- 0.701229

# Chave et al. (2005)
# the best dry forest equation if H was measured
# this model has been widely used previously but it is not recommended anymore
chave05.df <- function(dbh,h,wd) {
  exp( -2.187 +0.916*log(wd*(dbh^2)*h) )}

# compare AGB models using trees with height measurements
# different species are shown with different colours
t <- trees[is.na(trees$h)==F,] #only sample trees
pred.c14.4 <- chave14.4(t$dbh, t$h, t$wd.mean)
pred.c14.7 <- chave14.7(t$dbh, t$wd.mean, E)
pred.c05.df <- chave05.df(t$dbh, t$h, t$wd.mean)

par(mfrow=c(1,3))
plot(t$dbh, pred.c14.4, xlab="DBH (cm)", ylab="AGB (kg)", ylim=c(0,2000),
     main="Chave14, eq. 4 (DBH + H + WD)", col=as.factor(t$sp))
plot(t$dbh, pred.c14.7, xlab="DBH (cm)", ylab="AGB (kg)", ylim=c(0,2000),
     main="Chave14, eq. 7 (DBH + WD + E)", col=as.factor(t$sp))
plot(t$dbh, pred.c05.df, xlab="DBH (cm)", ylab="AGB (kg)", ylim=c(0,2000),
     main="Chave05, dry forest (DBH + H + WD)", col=as.factor(t$sp))

# the models based on the same inputs (DBH + H + WD) provide consistent results
# Chave14 models are in line with Chave05 dry forest equation
# if height is ignored (Chave14, eq.7), only variation due to species remains
# (WD depends on species, E is constant for the site)

# summaries for the predictions
summary(pred.c14.4)
summary(pred.c14.7)
summary(pred.c05.df)

# remove temporary data
rm(t, pred.c14.4, pred.c14.7, pred.c05.df)
graphics.off()


##########################
#COMPUTE AGB FOR EACH TREE AND PIECE OF DEAD WOOD
##########################

# Chave14 (eq.4) and heights based on "nlme5" (most accurate estimates) 
# are used in computations below.
# unit = kg

# trees, DBH > 10 cm
trees["agb"] <- chave14.4(trees$dbh, trees$h.nlme5, trees$wd.mean)

# trees, DBH 4-10 cm
l.trees.4_10["agb"] <- chave14.4(l.trees.4_10$dbh, l.trees.4_10$h,
                                 l.trees.4_10$wd.mean)
						 
# dead trees, DBH 4-10 cm
d.trees.4_10["agb"] <- chave14.4(d.trees.4_10$dbh, d.trees.4_10$h, d.wd)

# downed dead wood
# units: length in m, diameter(d) in cm
d.wood["agb"] <- pi*((d.wood$d/2)^2)*(d.wood$length/100) * d.wd
# take reduction factor into account:
d.wood$agb[d.wood$decay.class==1] <- d.wood$agb[d.wood$decay.class==1] * rf.1
d.wood$agb[d.wood$decay.class==2] <- d.wood$agb[d.wood$decay.class==2] * rf.2
d.wood$agb[d.wood$decay.class==3] <- d.wood$agb[d.wood$decay.class==3] * rf.3


#############################
#ALLOMETRIC MODEL FOR VOLUME
#############################

# Depending on the objective, volume (typically in m^3) can refer to total stem volume, 
# total tree volume (stem + branches) or volume above certain merchantable limit.
# Volume estimates can either include or exclude bark.
# Local volume models based on locally appropriate definition of volume should be used
# when such models are available.

# In the absense of local equations, volume can be estimate using generic volume function. 
# Form factor (F) can be adjusted according to the local knowledge.
# Magnussen and Reed (2004) recommend F=0.42
generic.f <- function(dbh,h,F) { F*(pi*(dbh/100/2)^2)*h }


#############################
#COMPUTE VOLUMES
#############################

# Generic model (F=0.42) and heights based on "nlme5" (most accurate estimates) 
# are used in computations below.
# unit = m^2

# trees, DBH > 10 cm
trees["v"] <- generic.f(trees$dbh, trees$h.nlme5, 0.42)

# trees, DBH 4-10 cm
l.trees.4_10["v"] <- generic.f(l.trees.4_10$dbh, l.trees.4_10$h, 0.42)

# dead trees, DBH 4-10 cm
d.trees.4_10["v"] <- generic.f(d.trees.4_10$dbh, d.trees.4_10$h, 0.42)

# downed dead wood
# convert d from cm to m in order to have output in m^3
d.wood["v"] <- pi*(((d.wood$d/100)/2)^2)*(d.wood$length)


#############################
#COMPUTE BELOWGROUND BIOMASS
#############################

# Belowground biomass (BGB) is computed as a product of root:shoot ratio and AGB.
# unit = kg

trees["bgb"] <- trees$agb * rs.mokany               #trees, DBH > 10 cm
l.trees.4_10["bgb"] <- l.trees.4_10$agb * rs.mokany #trees, DBH 4-10 cm
d.trees.4_10["bgb"] <- d.trees.4_10$agb * rs.mokany #dead trees, DBH 4-10 cm


#############################
#COMPUTE AMOUNT OF CARBON
#############################

# Amount of carbon is computed as a product of AGB/BGB and carbon fraction.
# unit = kg

# agc = aboveground carbon
trees["agc"] <- trees$agb * c.f               #trees, DBH > 10 cm
l.trees.4_10["agc"] <- l.trees.4_10$agb * c.f #trees, DBH 4-10 cm
d.trees.4_10["agc"] <- d.trees.4_10$agb * c.f #dead trees, DBH 4-10 cm
d.wood["agc"] <- d.wood$agb * c.f             #downed dead wood

# bgc = belowground carbon
trees["bgc"] <- trees$bgb * c.f               #trees, DBH > 10 cm
l.trees.4_10["bgc"] <- l.trees.4_10$bgb * c.f #trees, DBH 4-10 cm
d.trees.4_10["bgc"] <- d.trees.4_10$bgb * c.f #dead trees, DBH 4-10 cm


##########################
#ADD EXPANSION FACTORS
##########################

# Plot Expansion Factor (PEF) is ratio between 1 ha and the area where the tree was sampled.
# Hence, PEF is a function of plot (or subplot) radius.
# 1 ha = 10000 m^2
# PEF = 10000 m^2 / plot size in m^2

# function that computes PEF
# r is plot radius in m
PEF <-function (r) 10000 / round(pi*r^2,0)

# Hectare Expansion Factor (HEF) = relates each tree measurement directly (1) to the area 
# where it was sampled (PEF) and (2) the number of trees that measurement represents.

# in the case of trees DBH > 10 cm and palms, PEF = HEF
# add HEF column to trees table

# fixed plot size in BIODEV because radius was corrected in the field
trees$HEF <- PEF(17.84)

# in the case of the smaller plots (r=5.64), PEF needs to be multiplied by the number of trees
# and divided by 4, as each subplot is only 1/4 of the total sampling effort for the plot.
l.trees.4_10$HEF <- PEF(5.64) * l.trees.4_10$count / 4
d.trees.4_10$HEF <- PEF(5.64) * d.trees.4_10$count / 4
d.wood$HEF <- PEF(5.64) / 4


###############################################
#FINAL TREE LIST INCLUDING SUBPLOT MEASUREMENTS
###############################################

# Here, all the measurements are combined into single treelist.

# first, separate data frames need to be adapted so that they can be combined
# (i.e. have common column names)
common.fields <-c("plot.id","cluster","plot","dbh","height","species",
                  "tree.type","decay.class","agb","v","bgb","agc","bgc","HEF")

# trees, DBH > 10 cm
trees["height"] <- trees$h.nlme5 #selected height
trees["species"] <- trees$sp
trees["tree.type"] <- "living"
trees[trees$dead==1, "tree.type"] <- "SDT" #standing dead tree
trees["decay.class"] <- 0 #zero for all standing trees

# trees, DBH 4-10 cm
l.trees.4_10["height"] <- l.trees.4_10$h
l.trees.4_10["species"] <- l.trees.4_10$dominant.sp
l.trees.4_10["tree.type"] <- "living"
l.trees.4_10["decay.class"] <- 0

# dead trees, DBH 4-10 cm
d.trees.4_10["height"] <- d.trees.4_10$h
d.trees.4_10["species"] <- species[species$species=="Unknown","code"] #code for "unknown"
d.trees.4_10["tree.type"] <- "SDT" 
d.trees.4_10["decay.class"] <- 0 #zero for standing trees
  
# downed dead wood
d.wood["dbh"] <- d.wood$d #diameter in the middle
d.wood["height"] <- d.wood$length #length of the piece
d.wood["species"] <- species[species$species=="Unknown","code"] #code for "unknown"
d.wood["tree.type"] <- "DDW" #downed dead wood
d.wood["bgb"] <- 0 #BGB is not relevant for DDW
d.wood["bgc"] <- 0 #BGC is not relevant for DDW 

# next, combine common data from the data frames
TreeList <- rbind(trees[,common.fields],
                  l.trees.4_10[,common.fields],			
                  d.trees.4_10[,common.fields],
                  d.wood[,common.fields])

# finally, check the first rows of data
head(TreeList)


##########################
#SAVE TREELIST AS RDA-FILE
##########################

save(TreeList, file = "TreeList_Kou.rda")

