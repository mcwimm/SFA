################
### SAP FLOW ###
################

#### Description ####
sdDescriptionOutput <- function(){
   return(
      fluidRow(
         column(6,
                box(title = "Sap Flow Index", width = "100%",
                    collapsible = T, status = "info",
                    includeMarkdown("./man/des_sf_index.md")),
                box(title = "Sap Flow",
                    collapsible = T, status = "info", width = "100%",
                    includeMarkdown("./man/des_sf.md")),
                box(title = "References",
                    collapsible = T, status = "info", width = "100%",
                    p(strong("Sap flow index:"), 
                      HTML('&nbsp;'),
                      a("Nadezhdina et al. (2015)", 
                        href="https://content.sciendo.com/view/journals/johh/63/2/article-p124.xml",
                        target="_blank"), ",", 
                      HTML('&nbsp;'),
                      a("Nadezhdina (1999)", 
                        href="https://academic.oup.com/treephys/article/19/13/885/1623199", target="_blank")),
                    p(strong("Sap flow density:"), 
                      HTML('&nbsp;'),
                      a("Nadezhdina (2018)", 
                        href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ""))#,
                # HTML('&nbsp;'),
                # a("Author", href="", target="_blank")),
                # p(strong("Sap flow:"), 
                #   HTML('&nbsp;'),
                #   a("Author", href="", target="_blank"), ",",
                #   HTML('&nbsp;'),
                #   a("Author", href="", target="_blank")),
                # p(strong("Tree water use:"), 
                #   HTML('&nbsp;'),
                #   a("Author", href="", target="_blank"), ",",
                #   HTML('&nbsp;'),
                #   a("Author", href="", target="_blank")))
         ),
         
         
         column(6,
                box(title = "Sap Flow Density", width = "100%",
                    collapsible = T, status = "info",
                    includeMarkdown("./man/des_sf_density.md")),
                
                box(title = "Tree water use", width = "100%",
                    collapsible = T, status = "info",
                    includeMarkdown("./man/des_sf_twu.md"))
         )
         
      )
      
   )
}


#### Sap flow metrics ####

sfDensityOutput <- function(){
   return(list(
      fluidRow(
         box(title = "Figure settings",
             collapsible = T, width = 4,
             status = "warning",
             
             selectInput("sf_y_axis", "Y-axis",
                         choices = c("Sap flow index" = "SFI",
                                     "Sap flow per section" = "SFS",
                                     "Sap-wood-related density" = "SFDsw")),
             
             radioButtons("sf_style", "Style",
                          choices = c("Normal" = "normal",
                                      "Facet wrap" = "sf_facet_wrap",
                                      "Group by thermocouple positions" = 
                                         "sf_grouped")),
             
             conditionalPanel(
                condition = "input.sf_style == 'sf_facet_wrap'",
                
                radioButtons("sf_facet_scales","Scales", 
                             choiceNames =  list(
                                HTML("<span title='choose fixed'>fixed</span>"),
                                HTML("<span title='choose free'>free</span>")
                             ),
                             choiceValues = list("fixed", "free"),
                             inline=T),
                fluidRow(
                   column(6, selectInput("sf_facet_column", "Facet",
                                         choices = c("doy" = "doy",
                                                     "position" = "position"))),
                   column(6, numericInput("sf_facet_col_nums",
                                          "No. columns",
                                          value = 4))
                )),

             conditionalPanel(
                condition = "input.sf_style == 'sf_grouped'",
                
                p("Calculates mean of defined groups of thermocouples."),
                fluidRow(
                   column(8, textInput("sf_grouped_positions", "Groups*", 
                                       placeholder = "e.g. inner: 3,4; outer: 1,2,5 <enter>")),
                   column(4, textInput("sf_grouped_name", "Name",
                                       placeholder = "Group"))
                ),
                actButton("sf_grouped_go", "Render plot",
                          "update"),
                p("* provide the name of each group and the corresponding thermocouple 
                  positions separated by ':', list theromocouples separated by ',' 
                  and groups separated by ';', i.e. 'name: 2, 4'.")
             )
         ),
         box(title = "Figures",
             collapsible = T, width = 8,
             status = "info",
             tabsetPanel(
                tabPanel("Diurnal pattern", br(),
                         output.figure("sapFlowMetric"),
                         actButton("save.sapFlowMetricPlot",
                                   "Save figure", 
                                   "saveFigure"),
                         actButton("save.sapFlowMetrics", 
                                   "Save csv", "saveCsv")),
                tabPanel("Radial profile", br(),
                         output.figure("sapFlowMetric.RadialProfile"),
                         actButton("save.sapFlowMetric.RadialProfile",
                                   "Save figure", 
                                   "saveFigure"))
             ))
         )))
}

#### Sap flow rate ####

sfRateOutput <- function(){
   return(
      list(
         fluidRow(
            column(width = 4,
                   box(title = "Settings",
                       collapsible = T, width = NULL,
                       status = "warning",
                       
                       p(strong("<Note>"), "The estimation of sap flow is based
                                        wood properties (see 'Project settings') and 
                         the correct assignment of sensor positions (see 'Data | Upload')."),
                       
                       checkboxInput("treeScaleSimple1", 
                                     "Method 1: Area of circular ring", T),
                       checkboxInput("treeScaleSimple2", 
                                     "Method 2: Sapwood area", T),
                       checkboxInput("treeScaleSimple3", 
                                     "Method 3: Circumference of circular ring", T)
                   )),
            column(width = 8,
                   box(title = "Figures",
                       collapsible = T, width = NULL,
                       status = "info",
                       
                       tabsetPanel(
                          tabPanel("Diurnal pattern", br(),
                                   output.figure("SapFlowPlot"),
                                   actButton("save.SapFlow", "Save figure", "saveFigure"),
                                   actButton("save.SapFlowCsv", "Save csv", "saveCsv")),
                          tabPanel("Daily balance", br(),
                                   output.figure("SapFlowPlotBar"),
                                   actButton("save.SapFlowPlot",
                                             "Save figure", "saveFigure")))
                       
                   ),
                   box(title = "Tree water use",
                       p("Daily tree water use (in liter per day) estimated as the area under
                     the curve (AUC) of the figure above, separated by flow direction (i.e. positive flow rates = transpiration, negative flow rates = reverse flow)."),
                       p("Note: Incomplete data sets (e.g. half a day) might lead to an over- or underestimation."),
                       collapsible = T,
                       width = NULL,
                       status = "info",
                       output.table("twu.table"),
                       actButton("save.TreeWaterUseCsv", "Save csv", "saveCsv")
                   ))
         )))
}