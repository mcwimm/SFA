themes <- list("Bw" = theme_bw(),
               "Classic" = theme_classic(),
               "Grey" = theme_gray(),
               "Dark" = theme_dark(),
               "Light" = theme_light(),
               "Minimal" = theme_minimal(),
               "Void" = theme_void()
)
################
### SETTINGS ###
################
settingsOutput = function() {
  return(list(fluidRow(
    box(
      title = "Measuring environment",
      status = "warning",
      collapsible = T,
      width = 7,
      box.settings_measuring()
    ),
    box(
      title = "Project",
      status = "info",
      solidHeader = F,
      collapsible = T,
      width = 5,
      box.settings_project()
      
    ),
    box(
      title = "File output (optional)",
      status = "info",
      solidHeader = F,
      collapsible = T,
      collapsed = T,
      width = 5,
      box.setting_files()
    ),
    
    box(
      title = "Visualization (optional)",
      status = "info",
      solidHeader = F,
      collapsible = T,
      collapsed = T,
      width = 5,
      box.setting_visualization()
    )
  )))
}

box.settings_project = function(){
  return(list(
    p("1. Select or create a new folder to store files generated with this app."),
    shinyDirButton('folder', 
                   'Folder select', 
                   'Please select a folder', 
                   # multiple = FALSE,
                   style = buttonStyles("red"),
                   icon = icon("folder-open", style="margin-right:.5em")),
    
    p("2. Create your project directory."),
    actButton("crtPrj", "Create/set project", "create"),
    # br(), br(),
    h4("Current project"),
    verbatimTextOutput("prjName"),
    h4("Current project directory"),
    verbatimTextOutput("prjDir")
  ))
}

box.setting_files = function(){
  return(list(
    selectInput("figFor", "Figure format",
                c("jpg" = "jpg",
                  "rdata" = "rdata",
                  "pdf" = "pdf")),
    
    textInput("figTitle", "Figure title", 
              placeholder = "e.g. tree species"),
    radioButtons("fileAppend", "Prefix added to file names",
                 choices = c("Input file name" = "inputName",
                             "Manual" = "manual",
                             "None" = "none")),
    conditionalPanel(condition = "input.fileAppend == `manual`",
                     textInput("fileAppendName", "File name",
                               placeholder = "e.g. summer_22"))
  ))
}

box.setting_visualization = function(){
  return(list(
    selectInput("figTheme", "Figure theme (ggplot)",
                choices = names(themes),
                selected = themes["Light"]),
    uiOutput('theme_output'),
    textInput("fillColors", "Fill colors for discrete data*",
              placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00, #6495ED'),
    textInput("gradientColors", "Colors for gradient color scale**",
              placeholder = 'Hex colors, comma delimited: #CD5C5C, #FFBF00'),
    p("* Colors can be either hex colors or a RColorBrewer palette, 
                         e.g. 'Blues'"),
    p("** Two colors representing low and high values.")
  ))
}

box.settings_measuring = function(){
  return(list(
    h3(strong("Wood properties")),
    fluidRow(
      column(4, numericInput("stemCircumference", 
                             "Stem circumference (cm)",
                             value = 0.0)),
      column(4, numericInput("stemDiameter", 
                             "Stem diameter (cm)",
                             value = 0.0)),
      column(4, numericInput("barkThickness", 
                             "Bark thickness (cm)",
                             value = 0.0))
    ),
    
    fluidRow(
      column(4, numericInput("sapWoodDepth", 
                             "Sap wood depth (cm)",
                             value = 0.0)),
      column(4, numericInput("heartWoodDepth", 
                             "Heart wood depth (cm)",
                             value = 0.0))
    ),
    
    
    fluidRow(
      column(4, checkboxInput("swExact", "Use exact sap-/ heartwood values*",
                              F)),
      column(6, p("* If enabled wood attributes, i.e. R, Aring, Cring, are calculated using 
                            the sum of sapwood and heartwood depth"))
    ),
    
    
    
    numericInput("ThermalDiffusivity", 
                 HTML("Thermal diffusivity (cm<sup>2</sup> s <sup>-1</sup>)"),
                 value = 0.0025),
    
    
    box.settings_sensor()
  ))
}
box.settings_sensor = function(){
   return(list(
      h3(strong("Sensor properties")),
      fluidRow(
         column(6, numericInput("dist2first", 
                                "Distance to first thermometer (mm)",
                                value = 20)),
         column(6, numericInput("spacer", 
                                "Length of spacer (mm, distance sensor head to stem)",
                                value = 0))
      ),
      fluidRow(
         column(6, numericInput("Zax", 
                                "Axial sensor distance Zax (mm)",
                                value = 15)),
         column(6, numericInput("Ztg", 
                                "Tangential sensor distance Ztg (mm)",
                                value = 5))
      ),

      selectInput("sensorType", "Sensor type",
                  choices = c("HFD8-50", "HFD8-100", "Manual"),
                  selected = "HFD8-100"),
      
      
      conditionalPanel(
         condition = "input.sensorType == 'Manual'",
         numericInput("distInput", "Distance between thermometers (cm)",
                      value = 1),
         fluidRow(
            column(4, checkboxInput("positionManual", "Manual position input",
                                    F)),
            column(4, checkboxInput("depthManual", "Manual depth input",
                                    F))
         ),
         conditionalPanel(
            condition = "input.positionManual == true",
            textInput("positionInput", "Thermometer positions",
                      placeholder = "Thermometer positions as vector (comma delimited): 1, 2, 3")),
         
         conditionalPanel(
            condition = "input.depthManual == true",
            textInput("depthInput", "Thermometer depths",
                      placeholder = "Thermometer depths (cm) as vector (comma delimited): 
                   10, 8, 7.5")),
         ),
      
      p(strong("<Note>"), 
        "The following table shows estimated depth of thermometers as distance to the center (`Sensor R`) 
        as well as the area and circumference of the cirular ring, assuming the thermometers are centered 
        in the respective ring. Based on the values shown here, sap flow per section can be scaled to
        sap flow density or total tree water use."),
      p("Default: Position 1 is the outermost thermometer position (i.e. closest to the bark)."),
      
      output.table("depth.table"),
      
      p("* negative values for 'Sensor R' indicate that the thermometer is longer than 
        tree radius (diameter / 2 - barkthickness) and the respective thermometer positions are on the opposite side of the tree. "),
      p("** negative values for 'Area' indicate that the thermometers are located in the heartwood. Please adjust wood properties."),
      actButton("save.sensor_props", "Save csv", "saveCsv"),
      
      br(),
      p(strong("Schematic representation of an HFD sensor and its placement in the stem")),
      img(src='stemProfile.png', width = "100%")
      
   ))
}
