##############
## PACKAGES ##
##############

library("shiny")
library("shinydashboard")
library("shinycssloaders")  # enables loading signs
library("markdown")  # enables markdown format
library("DT")


# library("ggforce")  # geom_circle
# library("ggpubr")  # ggarrange
library("tidyverse")
# library("document")
# library("scales")  # scientif number format


##################
## MY FUNCTIONS ##
##################
path = "./R_scripts/functions/" # path to functions
modelFunctions = list.files(path)
lapply(modelFunctions, function(x) source(paste(path, x, sep = "")))


