################
### SAP FLOW ###
################

#### Description ####
sdDescriptionOutput <- function(){
   return(
      fluidRow(
         column(6,
                box(title = "Sap Flow Metrics", width = "100%",
                    collapsible = T, status = "info",
                    includeMarkdown("./man/des_sf_metrics.md"))
         ),
         
         
         column(6,
                box(title = "Tree water use", width = "100%",
                    collapsible = T, status = "info",
                    includeMarkdown("./man/des_sf_twu.md")),
                
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
                    p(strong("Sap flow per section and density:"), 
                      HTML('&nbsp;'),
                      a("Nadezhdina & Nadezhdin (2017)", 
                        href="https://doi.org/10.1016/j.envexpbot.2017.04.005", target="_blank"), ", ",
                      HTML('&nbsp;'),
                      a("Nadezhdina (2018)", 
                        href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ""),
                    p(strong("Sap flow and tree water use:"), 
                      HTML('&nbsp;'),
                      a("Nadezhdina (2018)", 
                        href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ", ",
                      HTML('&nbsp;'),
                      a("Nadezhdina (2012)", 
                        href="https://www.actahort.org/books/951/951_13.htm", target="_blank"), ""))
                
         )
         
      )
      
   )
}


#### Sap flow metrics ####

sfDensityOutput <- function(){
   return(list(
      fluidRow(
         box(title = "Settings",
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
                                                     "date" = "date",
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
             ),
             
             tags$hr(),
             radioButtons("sf_formula", "SFS-Formula (Nadezhdina & Nadezhdin, 2017)",
                          choiceNames = list("Positive", "Negative"),
                          choiceValues = list("Positive", "Negative"),
                           inline = TRUE),
             p(em("Default threshold for negative formula is SFI = 0\u00b0C.")),
             conditionalPanel(
                condition = "input.sf_formula == 'Negative'",
                textInput("sf_formula_threshold", "Manual threshold(s)*",
                          placeholder = "0, -0.1, -0.1"),
                p(em("*Enter either one threshold that will be applied to all thermometer positions, or one threshold for each position separated by comma (order: position 1, position 2, ...position n).")),
                actButton("sf_negative_go", "Render plot",
                          "update"),
             )
         ),
         box(title = "Figures",
             collapsible = T, width = 8,
             status = "success",
             checkboxInput("sapFlowMetric0flow", 
                           "Show zero-flow line (y=0)",
                           T),
             tabsetPanel(
                tabPanel("Diurnal pattern", br(),
                         output.figure("sapFlowMetric"),
                         actButton("save.sapFlowMetricPlot",
                                   "Save figure", 
                                   "saveFigure"),
                         actButton("save.sapFlowMetrics", 
                                   "Save file", "saveCsv")),
                tabPanel("Radial profile", br(),
                         output.figure("sapFlowMetric.RadialProfile"),
                         actButton("save.sapFlowMetric.RadialProfile",
                                   "Save figure", 
                                   "saveFigure")),
                tabPanel("Negative formula control plot", br(),
                         output.figure("sapFlowMetric.NegControl"),
                         actButton("save.sapFlowMetric.NegControl",
                                   "Save figure", 
                                   "saveFigure"))
             ))
         )))
}

#### Tree water use ####

sfRateOutput <- function(){
   return(
      list(
         fluidRow(
            column(width = 4,
                   box(title = "Settings",
                       collapsible = T, width = NULL,
                       status = "warning",
                       HTML("<b>Note:</b> The estimation of sap flow and tree water use is based on
                       wood and sensor properties (see 'Settings')."),
                       
                       checkboxInput("treeScaleSimple1", 
                                     "Method 1: Area of circular ring", F),
                       checkboxInput("treeScaleSimple2", 
                                     "Method 2: Sapwood area", F),
                       checkboxInput("treeScaleSimple3", 
                                     "Method 3: Circumference of circular ring", T)
                   )),
            column(width = 8,
                   box(title = "Figures",
                       collapsible = T, width = NULL,
                       status = "success",
                       
                       tabsetPanel(
                          tabPanel("Diurnal pattern", br(),
                                   output.figure("sapFlowTree"),
                                   actButton("save.sapFlowTree", "Save figure",
                                             "saveFigure"),
                                   actButton("save.SapFlowCsv", "Save file", "saveCsv")),
                          tabPanel("Daily balance", br(),
                                   output.figure("TWUbar"),
                                   actButton("save.TWUbarplot",
                                             "Save figure", "saveFigure")),
                          tabPanel("Radial profile", br(),
                                   p("Note: radial profile is not available for scaling method 2."),
                                   output.figure("TWUradialprofile"),
                                   actButton("save.TWUradialprofile",
                                             "Save figure", "saveFigure")))
                       
                   ),
                   box(title = "Tree water use",
                       p("Daily tree water use (in liter per day) is estimated as the area under
                     the curve (AUC) of the figure above, separated by flow direction (i.e. positive flow
                         rates = transpiration, negative flow rates = reverse flow)."),
                       p("Note: Incomplete data sets (e.g. half a day) might lead to an over- or underestimation."),
                       collapsible = T,
                       width = NULL,
                       status = "success",
                       output.table("TWUtable"),
                       actButton("save.TWUCsv", "Save file", "saveCsv")
                   ))
         )))
}