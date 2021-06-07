############
### DATA ###
############

### Upload ###

dataUplOutput = function(){
   return(list(
      fluidRow(
         box(title = "Description",
             collapsible = T,
             status = "info",
             includeMarkdown("./man/des_data.md")),
         box(title = "Upload file",
             collapsible = T,
             status = "warning",
             box.dat_upl.upload())),
      fluidRow(box(title = "Data",
                   collapsible = T,
                   status = "info",
                   
                   tabsetPanel(
                      tabPanel("wide", br(),
                               output.table("raw.wide")),
                      tabPanel("long", br(),
                               actButton("save_dat_upl", "Save csv", "saveCsv"),
                               br(),
                               output.table("raw.long")))),
               box(title = "Sensor settings (optional)",
                   collapsible = T,
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
      
      selectInput("inputType", "Input file type/ manufacturer",
                  c("ICT_raw" = "ICT_raw", 
                    "ICT_delta" = "ICT_delta")),
      numericInput("skip", "Skip:", min = 0, max = 100, 10),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      # Input: Select separator ----
      
      actButton("setData", "Use data", "create")
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
   return(list(
      fluidRow(
         box(title = "Subset data",
             collapsible = T, width = 4,
             status = "warning",
             actButton("LoadFilter", "Load filter options", "update"),
             uiOutput("filterOptions")
         ),
         
         box(title = "Figures",
             collapsible = T, width = 8,
             status = "info",
             box.filter.figures()
             
         ),
         column(
            width = 8, offset = 4,
            box(title = "Info",
                collapsible = T,
                width = NULL,
                status = "info",
                includeMarkdown("./man/des_data_filter.md")
            )
         )
      )
   ))
}

box.filter.figures = function(){
   return(list(
      fluidRow(
         column(4, checkboxInput("filterPlot_facetGrid", "Facet wrap", F)),
         column(4, 
                conditionalPanel(condition = "input.filterPlot_facetGrid == true",
                                 
                                 radioButtons("filterPlot_scales", "Scales", 
                                              inline = T,
                                              choices = c("fixed" = "fixed", 
                                                          "free" = "free")))
         )
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
          
          radioButtons("rawPlot_scales","Scales", 
                       choiceNames =  list(
                          HTML("<span title='choose free'>free</span>"),
                          HTML("<span title='choose fixed'>fixed</span>")
                       ),
                       choiceValues = list("free", "fixed"),
                       inline=T),
          conditionalPanel(
             condition = "input.rawPlot_scales == 'fixed'",
             
             sliderInput("rawPlot.x", "x-axis range",
                         min = -10, max = 10, step = 0.25,
                         value = c(-0.5, 1.5)),
             sliderInput("rawPlot.y", "y-axis range",
                         min = -10, max = 10, step = 0.25,
                         value = c(-2, 2))
          ),
          
          checkboxInput("rawPlot_facetWrap", "Facet wrap", F),
          conditionalPanel(
             condition = "input.rawPlot_facetWrap == true",
             
             selectInput("rawPlot.facet", "Facet",
                         choices = c("position" = "position",
                                     "day time" = "dTime",
                                     "doy" = "doy"))
          ),
          
          tags$hr(),
          p(strong("Optional settings only available for figure 'Variable'")),
          
          
          selectInput("rawPlot.xcol", "X-axis",
                      choices = c("dTsym.dTas" = "dTsym.dTas",
                                  "dTas" = "dTas",
                                  "dTsa" = "dTsa",
                                  "dTSym" = "dTSym")),
          selectInput("rawPlot.ycol", "Y-axis",
                      choices = c("dTas" = "dTas",
                                  "dTsa" = "dTsa",
                                  "dTSym" = "dTSym",
                                  "dTsym.dTas" = "dTsym.dTas")),
          selectInput("rawPlot.col", "Color",
                      choices = c("day time" = "dTime",
                                  "doy" = "doy",
                                  "position" = "position")),
          selectInput("rawPlot.shape", "Shape",
                      choices = c("position" = "position",
                                  "doy" = "doy"))
      ),
      box(title = "Figures",
          collapsible = T, width = 8,
          status = "info",
          tabsetPanel(
             tabPanel("Temperature difference", br(),
                      output.figure("deltaTfacetWrap"),
                      actButton("save.deltaTfacetWrap", "Save figure", "saveFigure")),
             tabPanel("Variable", br(),
                      output.figure("deltaTSingle"),
                      actButton("save.deltaTSingle", "Save figure", "saveFigure"))
          ))
   ))   
}

