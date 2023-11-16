##############
## PACKAGES ##
##############

if(!require("shiny")) install.packages("shiny")  
if(!require("shinydashboard")) install.packages("shinydashboard")  
if(!require("shinycssloaders")) install.packages("shinycssloaders") 
if(!require("shinyFiles")) install.packages("shinyFiles") 
if(!require("markdown")) install.packages("markdown")  
if(!require("DT")) install.packages("DT")  
if(!require("tidyverse")) install.packages("tidyverse") 
if(!require("viridisLite")) install.packages("viridisLite") # viridis options
if(!require("ggpubr")) install.packages("ggpubr") # draw regression line in ggplot
if(!require("scales")) install.packages("scales") # modify datetime format in ggpplot
if(!require("writexl")) install.packages("writexl") # write data frame to excel
if(!require("data.table")) install.packages("data.table") # write data frame to excel


##################
## MY FUNCTIONS ##
##################

# load all functions from ./R_scripts/functions/ directory
path = "./R_scripts/functions/" # path to functions
modelFunctions = list.files(path)
lapply(modelFunctions, function(x) source(paste(path, x, sep = "")))


