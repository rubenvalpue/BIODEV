# RUN ALL SCRIPTS
# Author: J.Heiskanen
###############################################################################


# This script can be used for updating all the results if changes are made
# to field data, allometric models, etc. 


##########################
#SET WORKING DIRECTORY
##########################

# clean the working environment and graphics
rm(list=ls()); graphics.off()

# specify path to your working directory
#setwd("C:/Temp/BIODEV/R/Data")
setwd("C:/HY-Data/JHHEISKA/documents/Temp/BIODEV codes/Kou/Data")


##########################
#RUN SCRIPTS
##########################

source("../Scripts/1_ReadCSV_Kou.R")
source("../Scripts/2_CheckData_Kou.R")
source("../Scripts/3_ImputeHeights_Kou.R")
source("../Scripts/4_Trees_Kou.R")
source("../Scripts/5_Plots_Kou.R")
source("../Scripts/6_CanopyCover_Kou.R")
source("../Scripts/7_MeansAndTotals_Kou.R")


# clean the working environment and graphics
rm(list=ls()); graphics.off()


# observe in the Windows file explorer how "Date modified" was changed



