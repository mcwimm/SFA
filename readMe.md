# Sap Flow Analyzer

The Sap Flow Analyzer (SFA) is an [R Shiny](https://shiny.rstudio.com/) App to process sap flow data recorded with Heat Field Deformation (HFD) instruments. The app allows to clean the data, determine k-values, calculate sap flow per section and density as well as to estimate sap flow rates and tree water use. A detailed description of the methodology can be found in Wimmler et al. (in prep.).

## User Guide

[Go to SFA User Guide](https://hackmd.io/@-Zyj5KK8QtCu0gqPrdZVGA/rkNXyruZs)

## Launch the App

#### Requirements

- [`R`](https://cran.r-project.org/)
- *optional* [`R Studio`](https://www.rstudio.com/)
- In `R`: shiny package
  + `if(!require("shiny")) install.packages("shiny")`
  
#### Option 1


- Download and open github repository via shiny function
  + ``runGitHub(repo = "SFA", username = "mcwimm", destdir = NULL)``

#### Option 2

- Clone or download the complete project (repo) manually
- RStudio: open _ui.R_ and click `Run App`
- Command line: R -e "shiny::runApp('~/shinyapp')"

For more information visit [Shiny](https://shiny.rstudio.com/articles/basics.html)
