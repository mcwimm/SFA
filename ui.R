source("set-up.R")


shinyUI(
    dashboardPage(
        skin = "green", 
        dashboardHeader(title = "Sap Flow Analyzer"),
        
        dashboardSidebar(
            #br(), br(), br(),
            sidebarMenu(menuOutput()),
            br(), br(), #br(),
            # br(), br(), br(),
            tags$hr(),
            tags$footer("Chair of Forest Biometrics", align = "center"),
            tags$footer("and", align = "center"),
            tags$footer("Systems Analysis", align = "center"),
            tags$footer("Technische Universität", align = "center"),
            tags$footer("Dresden", align = "center"),
            tags$footer("2020 - 2024", align = "center"),
            tags$hr()
            
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "about",
                        introOutput()),
                tabItem(tabName = "sett",
                        settingsOutput()),
                
                tabItem(tabName = "dat_upl",
                        dataUplOutput()),
                tabItem(tabName = "dat_filter",
                        dataFilterOutput()),
                tabItem(tabName = "dat_view",
                        fluidRow(dataViewOutput())),
                
                tabItem(tabName = "k_des",
                        kDescriptionOutput()),
                tabItem(tabName = "k_est",
                        kValueOutput()),
                
                tabItem(tabName = "sf_des",
                        sdDescriptionOutput()),
                tabItem(tabName = "sf_metrics",
                        sfDensityOutput()),
                tabItem(tabName = "sf_flow",
                        sfRateOutput()),
                
                tabItem(tabName = "uncert_i",
                        uncertaintyOutputI()),
                tabItem(tabName = "uncert_c",
                        uncertaintyOutputC())
            ),
            
            # style/ appearance
            fluidPage(
                tags$script(src = "project-settings.js"),
                tags$link(href = "styles.css",
                          rel = "stylesheet")
            )
        )
))
