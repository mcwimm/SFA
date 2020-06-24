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
            tags$footer("06-2020", align = "center"),
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
                tabItem(tabName = "sf_ind",
                        sfIndexOutput()),
                tabItem(tabName = "sf_dens",
                        sfDensityOutput()),
                tabItem(tabName = "sf_flow",
                        
                        fluidRow(
                            box(title = "Settings",
                                collapsible = T, width = 4,
                                status = "warning",

                                checkboxInput("treeScaleSimple1", "Method 1: SFD * Asd", F),
                                checkboxInput("treeScaleSimple2", "Method 2: SFS / swd", F),
                                checkboxInput("treeScaleSimple3", "Method 3: SFS * Csd", F)
                                
                            ),
                            box(title = "Figures",
                                collapsible = T, width = 8,
                                status = "info",
                                
                                output.figure("SapFlowPlot"),
                                actButton("save.SapFlow", "Save figures", "saveFigure")
                            )
                        ))
                    
            ),
            
            
            
            # style/ appearance
            fluidPage(
                tags$script(src = "project-settings.js"),
                tags$link(href = "styles.css",
                          rel = "stylesheet")
            )
        )
))
