####################
### K-ESTIMATION ###
####################

kDescriptionOutput <- function() {
   return(list(fluidRow(
      box(
         title = HTML("<i>K</i> estimation"),
         collapsible = T,
         status = "info",
         includeMarkdown("./man/des_k_value.md"),
         p(
            HTML('For more information see'),
            a("Nadezhdina (2018)", href = "https://iforest.sisef.org/abstract/?id=ifor2381-011", target =
                 "_blank")
         ),
      ),
      box(
         title = HTML("<i>K</i>-diagrams"),
         collapsible = T,
         status = "info",
         includeMarkdown("./man/des_k_diagrams.md"),
         img(src = 'K_diagram.jpeg', width = "80%"),
         includeMarkdown("./man/des_k_diagrams2.md"),
         img(src = 'K_controls.jpeg', width = "100%")
      )
   )))
}

kValueOutput <- function() {
   return(list(fluidRow(
      box(
         title = HTML("<i>K</i> estimation settings"),
         collapsible = T,
         status = "warning",
         box.k.estimation()
      ),
      box(
         title = "Control plots",
         collapsible = T,
         status = "success",
         radioButtons("k1Plot_scales", "Scales",
                      c(
                         "free" = F, "fixed" = T
                      ), inline = T),
         box.k.figures()
      )
   )))
}

box.k.estimation = function() {
   return(
      list(
         uiOutput("kPositionSelect"),
         
         tags$hr(),
         
         selectInput(
            "kMethod",
            "Method",
            choices = c(
               "no-flow regression" = "nf.regression",
               "no-flow median" = "nf.median",
               "manual" = "manual",
               "from csv" = "csv"
            )
         ),
         conditionalPanel(condition = "input.kMethod != `manual`",
                          verbatimTextOutput("kCurrent")),
         conditionalPanel(
            condition = "input.kMethod == `manual`",
            numericInput("kManual", "Enter k manually", value = 1.11)
         ),
         
         p(strong("No-flow regression: filter options")),
         
         checkboxInput(
            "kRegUseBoth",
            HTML(
               "Use <i>dTas</i> and <i>dTs-a</i> for regression (instead of solely <i>dTas</i>)"
            ),
            value = F
         ),
         
         checkboxInput("dTimeFilter", "Custom low flow time", F),
         conditionalPanel(condition = "input.dTimeFilter == true",
                          fluidRow(
                             column(
                                6,
                                numericInput(
                                   "kRegressionTime.start",
                                   label = "Start (0-24 h)",
                                   value = 22
                                )
                             ),
                             column(
                                6,
                                numericInput("kRegressionTime.end",
                                             label = "End (0-24 h)",
                                             value = 6)
                             )
                          )),
         checkboxInput(
            "kRegXFilter",
            HTML("Custom max. <i>dTsym dTas <sup>-1</sup></i>"),
            F
         ),
         conditionalPanel(condition = "input.kRegXFilter == true",
                          fluidRow(column(
                             6, numericInput(
                                "kRegXFilter.max",
                                label = HTML("max. <i>dTsym dTas <sup>-1</sup></i>"),
                                value = 1.5
                             )
                          ))),
         
         actButton("setK", HTML("Set <i>K</i>"), "setValue"),
         
         tabsetPanel(
            tabPanel(
               "Selected",
               output.table("kSelected"),
               actButton("save.kValues", "Save file", "saveCsv")
            ),
            tabPanel(
               "No-flow regression",
               output.table("kRegression"),
               actButton("setKfromRegression", HTML("Use <i>K</i> estimates"), "setValue")
            ),
            tabPanel(
               "No-flow median",
               output.table("kZeroFlow"),
               actButton("setKfromZeroFlow", HTML("Use <i>K</i> estimates"), "setValue")
            ),
            tabPanel(
               "Read csv",
               br(),
               # Input: Select a file ----
               fileInput(
                  "file2",
                  "Choose CSV File",
                  multiple = F,
                  accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"
                  )
               ),
               checkboxInput("moreOptions", "More upload options", F),
               conditionalPanel(
                  condition = "input.moreOptions == 1",
                  
                  fluidRow(column(
                     6, numericInput("skip2", "Skip:", min = 0,
                                     max = 100, 0)
                  ),
                  column(
                     6, checkboxInput("header2", "Header", TRUE)
                  )),
                  radioButtons(
                     "sep2",
                     "Separator",
                     inline = T,
                     choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                     ),
                     selected = ","
                  )
               ),
               output.table("uploadedKvalues"),
               actButton("setKfromCsv", HTML("Use <i>K</i> estimates"), "setValue")
            )
         )
      )
   )
}

box.k.figures = function() {
   return(list(
      uiOutput("xRangeSlider"),
      
      tabsetPanel(
         tabPanel(
            "K-diagram",
            br(),
            conditionalPanel(
               condition = "input.kMethod == `nf.regression`",
               checkboxInput("k1Plot.fullrange", "Fullrange regression",
                             value = F)
            ),
            output.figure("kvaluePlot1")
         ),
         tabPanel(
            "Control-diagram 1",
            br(),
            # conditionalPanel(
            #    condition = "input.kMethod == `regression`",
            #    checkboxInput(
            #       "k1Plot.forceOrigin",
            #       "Force regression through origin",
            #       value = F
            #    )
            # ),
            output.figure("kvaluePlot2")
         ),
         tabPanel("Control-diagram 2", br(),
                  output.figure("kvaluePlot3")),
         tabPanel("Diurnal flow", br(),
                  output.figure("kDiurnalPlot"))
      ),
      
      actButton("save.kPlots", "Save figures", "saveFigure")
   ))
}