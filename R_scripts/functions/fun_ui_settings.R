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
       title = "Save settings",
       status = "warning",
       collapsed = T,
       collapsible = T,
       width = 5,
       HTML("<b>Save</b> all settings to a .rds file.
         Settings include sensor and wood properties as well as file output and visualization options.
         The file can be used to restore these settings when the SFA is used again."),
       actButton("save.inputs", "Save settings", "saveCsv")
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
    selectInput("fileFor", "File format",
                c("csv" = "csv",
                  "xlsx" = "xlsx")),
    
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
                               placeholder = "e.g. summer_22"),
                     # Invisible text output to get individual name to header
                     span(textOutput("manualName"), style="color:white"))
  ))
}



box.setting_visualization = function(){
  return(list(
    selectInput("figTheme", "Figure theme (ggplot)",
                choices = names(themes),
                selected = themes["Light"]),
    uiOutput('theme_output'),
    selectInput("fillColors", "Color palette (viridis)*",
                choices = c("viridis", "magma", "plasma", "inferno",
                            "cividis", "mako", "rocket", "turbo")),

    HTML("*<a href='https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html' target='_blank'>Viridis documentation</a>")
  ))
}

box.settings_measuring = function(){
  return(list(
    h3(strong("Wood properties")),
    fluidRow(
       column(5, 
              numericInput("stemDiameter", 
                              "Stem diameter (d, cm)",
                              value = 0.0),
              numericInput("stemCircumference",
                                     "Stem circumference (c, cm)",
                                     value = 0.0),
              numericInput("barkThickness", 
                           "Bark thickness (bt, cm)",
                           value = 0.0)
              ),
       column(5, 
              numericInput("sapWoodDepth", 
                              "Sap wood depth (swd, cm)",
                              value = 0.0),
              checkboxInput("swExact", "Use exact sap-/ heartwood values*", F),
              conditionalPanel(
                 condition = "input.swExact == true",
                 numericInput("heartWoodDepth", 
                              "Heart wood depth (hwd, cm)",
                              value = 0.0)
              ),
              p("* If enabled wood attributes, i.e. R, Aring, Cring, are calculated using 
                            the sum of sapwood and heartwood depth"),
              offset = 1)
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
                                "Distance to first thermometer (lt, mm)",
                                value = 20)),
         column(6, numericInput("spacer", 
                                "Length of spacer (ls, mm, distance sensor head to stem)",
                                value = 0))
      ),
      fluidRow(
         column(6, numericInput("Zax", 
                                "Axial sensor distance (Zax, mm)",
                                value = 15)),
         column(6, numericInput("Ztg", 
                                "Tangential sensor distance (Ztg, mm)",
                                value = 5))
      ),

      selectInput("sensorType", "Sensor type",
                  choices = c("HFD8-50", "HFD8-100", "Manual"),
                  selected = "HFD8-100"),
      
      
      conditionalPanel(
         condition = "input.sensorType == 'Manual'",
         fluidRow(
           column(6,
                  radioButtons("thermoDistances", "Distances between thermometers",
                               choices = c("fixed" = "fixed",
                                           "variable" = "variable"))),
           column(6,
                  radioButtons("thermoNumbering", "Thermometer numbering",
                               choices = c("ascending" = "ascending",
                                           "descending" = "descending")))
         ),
         conditionalPanel(
           condition = "input.thermoDistances == 'fixed'",
           numericInput("distInput", "Distance between thermometers (cm)",
                        value = 1)
           ),
         conditionalPanel(
           condition = "input.thermoDistances == 'variable'",
           textInput("depthInput", "Thermometer depths",
                     placeholder = "Thermometer depths (cm) as vector (comma delimited): 
                   10, 8, 7.5")),
         ),

      h3(strong("Estimated thermometer positions")),
      
      p("The following table shows estimated depth of thermometers as distance to the center (`Thermometer R`) 
        as well as the area (`Area`) and circumference (`Circ.`) of the cirular ring, assuming the thermometers are centered 
        in the respective ring. Based on the values shown here, sap flow per section can be scaled to
        sap flow density or total tree water use."),
      p("Default: Position 1 is the outermost thermometer position (i.e. closest to the bark). To change this
        select sensor type 'manual'."),
      
      output.table("depth.table"),
      output.html("depth.table.info"),

      actButton("save.sensor_props", "Save file", "saveCsv"),
      
      br(),
      h4(strong("Schematic representation of an HFD sensor and its placement in the stem")),
      img(src='stemProfile.png', width = "100%")
      
   ))
}
