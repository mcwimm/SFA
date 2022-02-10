####################
### K-ESTIMATION ###
####################

kDescriptionOutput <- function(){
   return(list(
      fluidRow(
         box(title = "K-value estimation",
             collapsible = T,
             status = "info",
             includeMarkdown("./man/des_k_value.md")),
         box(title = "K-diagrams",
             collapsible = T,
             status = "info",
             includeMarkdown("./man/des_k_diagrams.md"),
             img(src='Nadezhdina_2018_fig1.png', width = "80%"),
             p(HTML('&nbsp;'),
               a("Nadezhdina (2018)", href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank")),
             includeMarkdown("./man/des_k_diagrams2.md")
         )  
      )
   ))
}

kValueOutput <- function(){
   return(list(
      fluidRow(
         box(title = "K-value estimation",
             collapsible = T, #width = 8,
             status = "warning",
             
             uiOutput("kPositionSelect"),
             
             tags$hr(),
             
             selectInput("kMethod", "Method",
                         choices = c("regression" = "regression",
                                     "no-flow" = "no.flow",
                                     "manual" = "manual",
                                     "csv" = "csv")),
             conditionalPanel(
                condition = "input.kMethod != `manual`",
                verbatimTextOutput("kCurrent")
             ),
             conditionalPanel(
                condition = "input.kMethod == `manual`",
                numericInput("kManual", "Enter k manually", value = 1.11)
             ),
             
             p(strong("Regression: filter options")),
             
             checkboxInput("kRegUseBoth", HTML("Use <i>dTas</i> and <i>dTs-a</i> for regression (instead of solely <i>dTas</i>)"), value = F),
            
             checkboxInput("dTimeFilter", "Custom no-flow time (regression)", F),
             conditionalPanel(
                condition = "input.dTimeFilter == true",
                fluidRow(
                   column(6, numericInput("kRegressionTime.start",
                                          label = "Start (0-24 h)",
                                          value = 22)),
                   column(6, numericInput("kRegressionTime.end", 
                                          label = "End (0-24 h)",
                                          value = 6))
                )
             ),
             checkboxInput("kRegXFilter", HTML("Custom max. <i>dTsym dTas <sup>-1</sup></i>  (regression)"), F),
             conditionalPanel(
                condition = "input.kRegXFilter == true",
                fluidRow(
                   column(6, numericInput("kRegXFilter.max",
                                          label = HTML("max. <i>dTsym dTas <sup>-1</sup></i>"),
                                          value = 1.5))
                )
             ),
             
             actButton("setK", "Set k-value", "setValue"),
             
             tabsetPanel(
                
                tabPanel("Selected",
                         output.table("kSelected"),
                         actButton("save.kValues", "Save csv", "saveCsv")),
                tabPanel("Regression",
                         output.table("kRegression"),
                         actButton("setKfromRegression", "Use k-values", "setValue")),
                tabPanel("No-flow",
                         output.table("kZeroFlow"),
                         actButton("setKfromZeroFlow", "Use k-values", "setValue")),
                tabPanel("Read csv", br(),
                         # Input: Select a file ----
                         fileInput("file2", "Choose CSV File",
                                   multiple = F,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         checkboxInput("moreOptions", "More upload options", F),
                         conditionalPanel(
                            condition = "input.moreOptions == 1",
                            
                            fluidRow(
                               column(6, numericInput("skip2", "Skip:", min = 0, 
                                                      max = 100, 0)),
                               column(6, checkboxInput("header2", "Header", TRUE))
                            ),
                            radioButtons("sep2", "Separator", inline = T,
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ",")),
                         output.table("uploadedKvalues"),
                         actButton("setKfromCsv", "Use k-values", "setValue")
                )
             )
         ),
         box(title = "Control plots",
             collapsible = T,
             status = "info",
             radioButtons("k1Plot_scales", "Scales", c("free" = F, "fixed" = T), inline=T),
             
             
             uiOutput("xRangeSlider"),
             
             tabsetPanel(
                tabPanel("K-diagram", br(),
                         conditionalPanel(
                            condition = "input.kMethod == `regression`",
                            checkboxInput("k1Plot.fullrange", "Fullrange regression", 
                                          value = F)
                         ),
                         output.figure("kvaluePlot1")),
                tabPanel("Control-diagram 1", br(),
                         conditionalPanel(
                            condition = "input.kMethod == `regression`",
                            checkboxInput("k1Plot.forceOrigin", 
                                          "Force regression through origin", 
                                          value = F)
                         ),
                         output.figure("kvaluePlot2")),
                tabPanel("Control-diagram 2", br(),
                         output.figure("kvaluePlot3")),
                tabPanel("Diurnal flow", br(),
                         output.figure("kDiurnalPlot"))
             ),
             
             actButton("save.kPlots", "Save figures", "saveFigure")
         )
      )))
}

