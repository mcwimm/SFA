# Sap Flow Analyser

The Sap Flow analyser is an R Shiny app to process sap flow data recorded with HFD (Heat Field Defomation) sensors. The app allows to clean the data, determine the k-value, calculate sap flow index and sap flow density and to estimate sap flow rates and tree water use if tree properties are given.

## Installation

### ... in R: Option 1

requires `shiny` package

``runGitHub(repo = "SFA", username = "mcwimm", destdir = NULL)``

``'destdir`` ... destination directory. If ``NULL`` this is the temporary directory (Caution: files will be deleted)

### ... in R: Option 2

1. fork/ clone the complete project (repo)
2. open ui.R with R and click Run App

