################
### SAP FLOW ###
################

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

sfIndexOutput <- function(){
   return(list(
      fluidRow(
         box(title = "Settings",
             collapsible = T, width = 4,
             status = "warning",
             # ToDO: Conditional panel not working
             
             radioButtons("sfIndexPlot_scales","Scales", 
                          choiceNames =  list(
                             HTML("<span title='choose free'>free</span>"),
                             HTML("<span title='choose fixed'>fixed</span>")
                          ),
                          choiceValues = list("free", "fixed"),
                          inline=T),
             conditionalPanel(
                condition = "input.sfIndexPlot_scales == 'fixed'",
                
                sliderInput("sfIndexPlot.x", "Time slider (0-24 h), only available for 'Daily'-figure",
                            min = 0, max = 24, step = 0.25,
                            value = c(0, 24)),
                sliderInput("sfIndexPlot.y", "y-axis range",
                            min = -10, max = 10, step = 0.25,
                            value = c(-0.5, 2))
             ),
             
             
             checkboxInput("sfIndexPlot_wrap", "Facet wrap", T),
             conditionalPanel(
                condition = "input.sfIndexPlot_wrap == true",
                
                selectInput("sfIndexPlot.facet", "Facet",
                            choices = c("doy" = "doy",
                                        "position" = "position"))
             )
             
         ),
         box(title = "Sap Flow Index",
             collapsible = T, width = 8,
             status = "info",
             tabsetPanel(
                tabPanel("Complete", br(),
                         output.figure("sapFlowIndex"),
                         actButton("save.sfIndex", "Save figure", "saveFigure")),
                tabPanel("Daily", br(),
                         output.figure("sapFlowIndex.Day"),
                         actButton("save.sfIndex.day", "Save figure", "saveFigure"))
             ))
      )))
}


sfDensityOutput <- function(){
   return(list(
      fluidRow(
         box(title = "Figure settings",
             collapsible = T, width = 4,
             status = "warning",
             
             checkboxInput("sapFlowDensityPlot_facetWrap", "Facet wrap", F),
             conditionalPanel(
                condition = "input.sapFlowDensityPlot_facetWrap == true",
                radioButtons("sapFlowDensityPlot_scales","Scales", 
                             choiceNames =  list(
                                HTML("<span title='choose free'>free</span>"),
                                HTML("<span title='choose fixed'>fixed</span>")
                             ),
                             choiceValues = list("free", "fixed"),
                             inline=T),
                selectInput("sapFlowDensityPlot.facet", "Facet",
                            choices = c("doy" = "doy",
                                        "position" = "position"))),
             
             selectInput("sapFlowDensityPlot.y", "Y-axis",
                         choices = c("Sap flow per section" = "SFS",
                                     "Sap-wood-related density" = "SFDsw")),
             selectInput("sapFlowDensityPlot.color", "Color",
                         choices = c("position" = "position",
                                     "doy" = "doy"))
         ),
         box(title = "Figure",
             collapsible = T, width = 8,
             status = "info",
             output.figure("sapFlowDensity"),
             actButton("save.sapFlowDensityPlot", "Save figure", "saveFigure"),
             actButton("save.sapFlowDensity", "Save csv", "saveCsv")
         )
      )))
}

sfRateOutput <- function(){
   return(
      list(
         fluidRow(
            column(width = 4,
                   box(title = "Settings",
                       collapsible = T, width = NULL,
                       status = "warning",
                       
                       p(strong("<Note>"), "The estimation of sap flow is based
                                        on sensor positions (see 'Data | Upload') and wood
                                        properties (see 'Project settings')."),
                       
                       checkboxInput("treeScaleSimple1", "Method 1: SFD * Asd", T),
                       checkboxInput("treeScaleSimple2", "Method 2: SFS / swd", T),
                       checkboxInput("treeScaleSimple3", "Method 3: SFS * Csd", T)
                   )),
            column(width = 8,
                   box(title = "Figures",
                       collapsible = T, width = NULL,
                       status = "info",
                       
                       output.figure("SapFlowPlot"),
                       actButton("save.SapFlow", "Save figures", "saveFigure"),
                       actButton("save.SapFlowCsv", "Save csv", "saveCsv")
                       
                   )),
            column(
               width = 8, offset = 4,
               box(title = "Tree water use",
                   p("Daily tree water use (in liter per day) estimated as the area under
                     the curve (AUC) of the figure above."),
                   collapsible = T,
                   width = NULL,
                   status = "info",
                   output.table("twu.table"),
                   actButton("save.TreeWaterUseCsv", "Save csv", "saveCsv")
               ))
         )
      )
   )
}