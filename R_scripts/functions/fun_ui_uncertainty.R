###################
### uncertainty ###
###################

uncertaintyOutputI = function() {
   return(list(
      box(
         title = "Settings",
         collapsible = T,
         width = 4,
         status = "warning",
         output.html("uncertaintyInputs"),
         
         numericInput("uncertaintyRange",
                      label = "\u00b1Error in input (0-100 %)",
                      value = 40),
         
         numericInput("uncertaintySteps",
                      label = "No. of steps",
                      value = 5),
         
         selectInput(
            "uncert_y",
            "Variable",
            choices = c(
               "Sap flow per section" = "SFS",
               "Sap-wood-related density" = "SFD",
               "Sap flow" = "SF",
               "Tree water use" = "TWU"
            )
         ),
         conditionalPanel(
            condition = "input.uncert_y == 'SF' | input.uncert_y == 'TWU'",
            
            radioButtons(
               "uncert_y_method",
               "Scaling method",
               choiceNames =  list("1", "2", "3"),
               choiceValues = list(
                  "treeScaleSimple1",
                  "treeScaleSimple2",
                  "treeScaleSimple3"
               ),
               inline = T
            )
         )
      ),
      box(
         title = "Results",
         collapsible = T,
         width = 8,
         status = "success",
         tags$style(HTML(
            ".tabbable > .nav > li > a {margin-top:5px;}"
         )),
         tabsetPanel(
            tabPanel("Absolute",
                     tabsetPanel(
                        tabPanel(
                           "Figure",
                           output.figure("uncertaintyPlot"),
                           actButton("save.uncertaintyPlot", "Save figure", "saveFigure")
                        ),
                        tabPanel(
                           "Table",
                           output.table("uncertaintyOutputs"),
                           actButton("save.uncertaintyOutputs", "Save results", "saveCsv")
                        )
                     )),
            tabPanel("Relative",
                     tabsetPanel(
                        tabPanel(
                           "Figure",
                           output.figure("uncertaintyPlotRel"),
                           actButton("save.uncertaintyPlotRel", "Save figure", "saveFigure")
                        ),
                        tabPanel(
                           "Table",
                           output.table("uncertaintyOutputsRel"),
                           actButton("save.uncertaintyOutputsRel", "Save results", "saveCsv")
                        )
                     )),
         )
      )
   ))
}


