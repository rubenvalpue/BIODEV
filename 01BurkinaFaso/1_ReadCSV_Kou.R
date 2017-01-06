# READ BIODEV TREE MEASUREMENTS FILE AND PREPARE DATA
# Authors: R.Valbuena, J.Heiskanen & P.Packalen  
###############################################################################


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment and graphics
rm(list=ls()); graphics.off()

# specify path to your working directory
# working directory will remain the same until you close R or change it
setwd("C:/Temp/BIODEV/R/Data")

# return filepath of the current working directory 
getwd()


##########################
#IMPORT CSV-FILES
##########################

# list CSV-files in the working directory
list.files(,".csv")

# import data sheets
# "Plots"
plots <- read.csv("Plots.csv", sep=",", header=TRUE)
plots <- plots[,1:9]
colnames(plots) <- c("date","cluster","plot","lat","lon","error","elev","slope",
	"radius")

# "Trees DBH > 10 cm"
trees <- read.csv("Trees DBH gt 10 cm.csv", sep=",", header=TRUE)
trees <- trees[,1:7]
colnames(trees) <- c("cluster","plot","tree","sp","dbh","unusual","dead")

# "Sample trees DBH > 10 cm"
s.trees <- read.csv("Sample trees DBH gt 10 cm.csv", sep=",", header=TRUE)
s.trees <- s.trees[,1:7]
colnames(s.trees) <- c("cluster","plot","tree","dbh","h","cd1","cd2")

# "Living trees DBH 4-10 cm"
l.trees.4_10 <- read.csv("Living trees DBH 4-10 cm.csv", sep=",", header=TRUE)
l.trees.4_10 <- l.trees.4_10[,1:10]
colnames(l.trees.4_10) <- c("cluster","plot","subplot","count","dbh","h","cd1",
				"cd2","dominant.sp","other.sp")

# "Standing dead trees DBH 4-10 cm"
d.trees.4_10 <- read.csv("Standing dead trees DBH 4-10 cm.csv", sep=",",
	header=TRUE)
d.trees.4_10 <- d.trees.4_10[,1:5]
colnames(d.trees.4_10) <- c("cluster","plot","subplot","count","dbh")

# "Dead wood lying on the ground"
d.wood <- read.csv("Dead wood lying on the ground.csv", sep=",", header=TRUE)
d.wood <- d.wood[,1:7]
colnames(d.wood) <- c("cluster","plot","subplot","id","length","d","decay.class")

# "Species list" and wood densities
# List includes only tree species which were observed in the field and 
# wood densities derived from literature and online databases 
species <- read.csv("Species list.csv", sep=",", header=TRUE)
species <- species[,1:3]
colnames(species) <- c("code","name","wd.mean")


##########################
#ADD UNIQUE PLOT IDENTIFIERS
##########################

# Each plot has unique identifier that consist of cluster and plot numbers.
# Because single field plot id would simplify other computations, we add 
# such id by combining cluster and plot numbers (plot.id = cluster*100+plot,
# e.g. cluster=10, plot=9, plot.id = 1009).

plots["plot.id"] <- plots$cluster*100 + plots$plot
trees["plot.id"] <- trees$cluster*100 + trees$plot 
s.trees["plot.id"] <- s.trees$cluster*100 + s.trees$plot
l.trees.4_10["plot.id"] <- l.trees.4_10$cluster*100 + l.trees.4_10$plot 
d.trees.4_10["plot.id"] <- d.trees.4_10$cluster*100 + d.trees.4_10$plot
d.wood["plot.id"] <- d.wood$cluster*100 + d.wood$plot


##########################
#SAVE INPUT FILES AS RDA-FILES
##########################

# Finally, input files are saved as RData files

# "Plots"
save(plots, file = "Kou_input_plots.rda")
# "Trees DBH > 10 cm"
save(trees, file = "Kou_input_trees.rda")
# "Sample trees DBH > 10 cm"
save(s.trees, file = "Kou_input_sampleTrees.rda")
# "Living trees DBH 4-10 cm"
save(l.trees.4_10, file = "Kou_input_living.rda")
# "Standing dead trees DBH 4-10 cm"
save(d.trees.4_10, file = "Kou_input_snags.rda")
# "Dead wood lying on the ground"
save(d.wood, file = "Kou_input_dead.rda")
# "Species list"
save(species, file = "Kou_input_species.rda")

