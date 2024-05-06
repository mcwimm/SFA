# Sap Flow Analyzer

The Sap Flow Analyzer (SFA) is an [R Shiny](https://shiny.rstudio.com/) App to process sap flow data recorded with Heat Field Deformation (HFD) instruments. The app allows to clean the data, determine k-values, calculate sap flow per section and density as well as to estimate sap flow rates and tree water use. A detailed description of the methodology can be found in Wimmler et al. (in prep.).

## User Guide

[Go to SFA User Guide](https://hackmd.io/@-Zyj5KK8QtCu0gqPrdZVGA/rkNXyruZs)

## Launch the App

#### Requirements

- [`R`](https://cran.r-project.org/)
- *optional* [`R Studio`](https://www.rstudio.com/)

  
#### Option 1

Run the following lines in R to use the latest version of the app

```
if(!require("shiny")) install.packages("shiny")
runGitHub(repo = "SFA", username = "mcwimm", destdir = NULL)
```

*Note*
This option will download and open the application directly from the github repository.
This means that each time you run this command, the application will be freshly downloaded to your computer.

#### Option 2

- Clone or download the complete project (repo) manually
- RStudio: open _ui.R_ and click `Run App`
- Command line: ``R -e "shiny::runApp('~/shinyapp')"``

For more information visit our [User Guide](https://hackmd.io/@-Zyj5KK8QtCu0gqPrdZVGA/rkNXyruZs) or the [Shiny](https://shiny.rstudio.com/articles/basics.html) Webpage.
