############
### DATA ###
############

### Upload ###
dataUplOutput = function(){
   return(
      fluidRow(
         column(6,
                box(title = "Description",
                    collapsible = T, width = "100%",
                    status = "info",
                    includeMarkdown("./man/des_data.md")),
                box(title = "Preview data",
                    collapsible = T, width = "100%",
                    status = "info",
                    
                    tabsetPanel(
                       tabPanel("wide", br(),
                                output.table("raw.wide")),
                       tabPanel("long", br(),
                                actButton("save_dat_upl", "Save csv", "saveCsv"),
                                br(),
                                output.table("raw.long"))))),
         column(6,
                box(title = "Upload file",
                    collapsible = T, width = "100%",
                    status = "warning",
                    box.dat_upl.upload()
                    ),
                box(title = "Sensor settings (optional)",
                    collapsible = T, width = "100%",
                    status = "warning",
                    box.dat_upl.depths()
                    )
                )
      ))
}

box.dat_upl.upload = function(){
   return(list(
      # Input: Select a file ----
      fluidRow(
         column(6, fileInput("file1", "Choose CSV File",
                             buttonLabel = HTML("<span 
                class='btn btn-primary' 
                style='margin: -8px -13px;
                  position: relative;
                  top: -2px;
                  border-radius: 0;margin: -8px -13px;
                   position: relative;
                   top: -2px;
                   border-radius: 0;'>
                                   Browse...
                                   </span>"),
                             multiple = F,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))),
         column(6,  selectInput("sep", "Separator", 
                                choices = c("Semicolon" = ";",
                                            "Comma" = ",",
                                            "Tab" = "\t")))
      ),
      
      selectInput("inputType", "Input file type",
                  c("Raw" = "HFD_raw", 
                    "Delta" = "HFD_delta")),
      numericInput("skip", "Skip:", min = 0, max = 100, 10),

      # Input: Checkbox if file has header ----
      # checkboxInput("header", "Header", TRUE),
      # Input: Select separator ----
      
      actButton("setData", "Use data", "create"),
      p(em("<Note> If your data set contains non-numeric rows, e.g. logger warnings,
           they are converted to NA values. Remove them in the 'Data > Filter' section."))
   ))
}

box.dat_upl.depths = function(){
   return(list(
      p(strong("<Note>"), 
        "Here, optional settings on sensor positions and the resulting stem
        radius can be defined. Therefore, information on wood properties (see
        'Project Settings') and sensor distances are required. Based on the sensor
        position the area and circumference
        of the circular ring, which are required to calculate sap flow rates,
        are estimated."),
      p("Default: Position 1 is the outermost sensor position (i.e. closest to the bark) 
      if not defined otherwise."),
      fluidRow(
         column(4, checkboxInput("positionManual", "Manual position input",
                                 F)),
         column(4, checkboxInput("depthManual", "Manual depth input",
                                 F))
      ),
      
      conditionalPanel(
         condition = "input.positionManual == true",
         textInput("positionInput", "Sensor positions",
                   placeholder = "Sensor positions as vector (comma delimited): 1, 2, 3")),
      
      conditionalPanel(
         condition = "input.depthManual == true",
         textInput("depthInput", "Sensor depths",
                   placeholder = "Sensor depths (cm) as vector (comma delimited): 
                   10, 8, 7.5")),
      
      
      selectInput("sensorType", "Sensor type",
                  choices = c("HFD8-50", "HFD8-100", "Manual"),
                  selected = "HFD8-100"),
      
      
      conditionalPanel(
         condition = "input.sensorType == 'Manual'",
         numericInput("distInput", "Distance between sensors (cm)",
                      value = 1)),
      
      
      
      # verbatimTextOutput("depths"),
      output.table("depth.table"),
      
      p("* negative values for 'Sensor R' indicate that the sensor is longer than 
        tree radius (DBH / 2 - barkthickness) and the respective sensor positions are on the opposite side of the tree. "),
      p("** negative values for 'Area' indicate that the sensors are located in the heartwood. Please adjust wood properties."),
      
      br(),
      p(strong("Schematic representation of an HFD sensor and its placement in the stem")),
      img(src='stemProfile.png', width = "100%")
      
   ))
}

### Filter ###

dataFilterOutput = function(){
   return(
      fluidRow(
         column(4, box(title = "Filter options",
                       collapsible = T,  width = "100%",
                       status = "warning",
                       actButton("LoadFilter", "Load filter options", "update"),
                       uiOutput("filterOptions")
         )),
         column(8,
                box(title = "Figures",
                    collapsible = T, width = "100%",
                    status = "info",
                    box.filter.figures(),
                    actButton("save_dat_filter", "Save csv", "saveCsv"),
                    actButton("save_dat_filter_fig", "Save figure", "saveFigure")),
                box(title = "Info",
                    collapsible = T, width = "100%",
                    status = "info",
                    includeMarkdown("./man/des_data_filter.md"))
                
      ))
   )
}

box.filter.figures = function(){
   return(list(
      fluidRow(
         column(4, checkboxInput("filterPlot_facetGrid", 
                                 "Facet grid (doy ~ position)", F))
      ),
      
      radioButtons("filterPlot_type", "Diagram type", inline = T,
                   choices = c("Violin plot" = "violin",
                               "Boxplot" = "boxp",
                               "Histogram" = "hist", 
                               "Frequency polygons" = "freq")),
      
      fluidRow(
         column(4, selectInput("filterPlot_X", "Variable",
                               choices = c("dTsym.dTas" = "dTsym.dTas",
                                           "dTas" = "dTas",
                                           "dTsa" = "dTsa",
                                           "dTSym" = "dTSym"))),
         column(4, selectInput("filterPlot_col", "Color",
                               choices = c("doy" = "doy",
                                           "none" = "none",
                                           "position" = "position"))),
         column(4, numericInput("filterPlot_binwidth", "Binwidth", value = 0.1))
      ),
      
      output.figure("filterPlot")
   ))
}

### View ###

dataViewOutput = function(){
   return(list(
      box(title = "Settings",
          collapsible = T, width = 4,
          status = "warning",

          fluidRow(
             column(6, selectInput("rawPlot.xcol", "X-axis",
                                  choices = c("dTsym.dTas" = "dTsym.dTas",
                                              "dTas" = "dTas",
                                              "dTsa" = "dTsa",
                                              "dTSym" = "dTSym",
                                              "day time" = "dTime"))),
             column(6, selectInput("rawPlot.ycol", "Y-axis",
                                  choices = c("dTas" = "dTas",
                                              "dTsa" = "dTsa",
                                              "dTSym" = "dTSym",
                                              "dTsym.dTas" = "dTsym.dTas")))
          ),
          fluidRow(
             column(6, selectInput("rawPlot.col", "Color",
                                   choices = c("day time" = "dTime",
                                               "doy" = "doy",
                                               "position" = "position"))),
             column(6, selectInput("rawPlot.shape", "Shape",
                                   choices = c("position" = "position",
                                               "doy" = "doy")))
          ),
          
          checkboxInput("rawPlot_facetWrap", "Facet wrap", F),
          
          conditionalPanel(
             condition = "input.rawPlot_facetWrap == true",
             radioButtons("rawPlot_scales","Scales", 
                          choiceNames =  list(
                             HTML("<span title='choose fixed'>fixed</span>"),
                             HTML("<span title='choose free'>free</span>")
                          ),
                          choiceValues = list("fixed", "free"),
                          inline=T),
             fluidRow(
                column(6, selectInput("rawPlot.facet", "Facet",
                                      choices = c("position" = "position",
                                                  "doy" = "doy"))),
                column(6, numericInput("rawPlot.columns", "No. columns",
                                       value = 3))
             )
          ),
          tags$hr(),
          
          checkboxInput("rawPlot_gathered", "Plot all temperature differences over time", F),
          
          
          actButton("renderPlot", "Render figure", "update")
      
      ),
      box(title = "Figure",
          collapsible = T, width = 8,
          status = "info",
          output.figure("custumPlot"),
          actButton("save.custumPlot", "Save figure", "saveFigure"))
   ))   
}

