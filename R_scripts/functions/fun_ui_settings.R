
################
### SETTINGS ###
################

settingsOutput = function(){
   return(
      fluidRow(
         column(8,
                fluidRow(
                   box(title = "Project",
                       status = "warning", solidHeader = F, #height = 300,
                       collapsible = T, width = 12,
                       
                       p("Please select a project or create a new folder:"),
                       
                       shinyDirButton('folder', 
                                      'Folder select', 
                                      'Please select a folder', 
                                      # multiple = FALSE,
                                      style = buttonStyles("red"),
                                      icon = icon("folder-open")),
                       actButton("crtPrj", "Create/set project", "create"),
                       br(), br(),
                       h4("Current project"),
                       verbatimTextOutput("prjName"),
                       h4("Current project directory"),
                       verbatimTextOutput("prjDir")
                   ),
                   box(title = "Measuring environment",
                       status = "warning",
                       collapsible = T, width = 12,
                       h4("Wood properties"),
                       
                       fluidRow(
                          column(4, numericInput("sapWoodDepth", 
                                                 "Sap wood depth (cm)",
                                                 value = 0.0)),
                          column(4, numericInput("heartWoodDepth", 
                                                 "Heart wood depth (cm)",
                                                 value = 0.0)),
                          column(4, numericInput("barkThickness", 
                                                 "Bark thickness (cm)",
                                                 value = 0.0))
                       ),
                       
                       fluidRow(
                          column(6, numericInput("stemCircumference", 
                                                 "Stem circumference (cm)",
                                                 value = 0.0)),
                          column(6, numericInput("stemDiameter", 
                                                 "Stem diameter (cm)",
                                                 value = 0.0))
                       ),
                       
                       numericInput("ThermalDiffusivity", 
                                    "Thermal diffusivity (cm/s)",
                                    value = 0.0025),
                       
                       h4("Sensor properties"),
                       fluidRow(
                          column(6, numericInput("Zax", 
                                                 "Axial sensor distance Zax (mm)",
                                                 value = 15)),
                          column(6, numericInput("Ztg", 
                                                 "Tangential sensor distance Ztg (mm)",
                                                 value = 5))
                       )
                   )
                )),
         column(4,
                fluidRow(
                   box(title = "Output",
                       status = "info", solidHeader = F, 
                       collapsible = T, width = 12,
                       p("These inputs are optional."),
                       selectInput("figFor", "Figure format",
                                   c(#"svg" = "svg",
                                      "pdf" = "pdf",
                                      "jpg" = "jpg")),
                       checkboxInput("prjNameAsTitle",
                                     "Use project name as title", value = T),
                       conditionalPanel(
                          condition = "input.prjNameAsTitle != true",
                          textInput("figTitle", "Figure title", 
                                    placeholder = "Tree species")
                       ),
                       p(strong("Visualization")),
                       p("NOT WORKING!!!", style = "color:red"),
                       
                       textInput("gradientColorLow", 
                                 "Gradient color low", 
                                 placeholder = "#d8b365"),
                       textInput("gradientColorHigh", 
                                 "Gradient color high", 
                                 placeholder = "#5ab4ac")
                       
          )))
      )
   )
}