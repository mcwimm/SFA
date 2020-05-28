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



source("set-up.R")


shinyUI(
    dashboardPage(
        skin = "black", 
        dashboardHeader(title = "Sap Flow Analyzer"),
        
        dashboardSidebar(
            br(), br(), br(),
            sidebarMenu(menuOutput()),
            br(), br(), br(), tags$hr(),
            tags$footer("By Marie-Christin Wimmler", align = "center"),
            tags$footer("05-2020", align = "center"),
            tags$hr()
            
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "about",
                        introOutput()),
                tabItem(tabName = "sett",
                        settingsOutput()),
                
                tabItem(tabName = "dat_upl",
                        fluidRow(dataUplOutput())),
                tabItem(tabName = "dat_view",
                        fluidRow(dataViewOutput())),
                
                tabItem(tabName = "k_des",
                        includeMarkdown("./man/des_k-value.md")),
                
                tabItem(tabName = "sf_ind",
                        sfIndexOutput()
                )
                    
            ),
            
            
            
            # style/ appearance
            fluidPage(
                tags$script(src = "project-settings.js"),
                tags$link(href = "styles.css",
                          rel = "stylesheet")
            )
        )
))
