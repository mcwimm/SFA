###############
### GENERAL ###
###############

output.table = function(outputID){
   return(list(br(), 
          DT::dataTableOutput(outputID) %>% 
             withSpinner(color="#0dc5c1")))
}

output.figure = function(outputID){
   return(plotOutput(outputID) %>% withSpinner(color="#0dc5c1"))
}

#############
### STYLE ###
#############

actButton <- function(ID, label, type){
   if (type == "saveCsv"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                   icon("file-download")))
   }
   if (type == "saveFigure"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                   icon("file-download")))
   }
   if (type == "setValue"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("red"), "margin-bottom: 2rem", sep = ";"),
                   icon("check-circle")))
   }
   if (type == "create"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("red"), "margin-bottom: 2rem", sep = ";"),
                   icon("folder-plus")))
   }
   if (type == "update"){
      return(actionButton(ID, label,
                   style = buttonStyles("green"), 
                   icon("broom")))
   }
   
}

buttonStyles = function(type = "blue"){
   if (type == "blue")
   { ##14B3EE#337ab7
      return("color: #fff; background-color: #14B3EE; border-color: #2e6da4; margin-bottom: 2rem; margin-top: 2rem")
   }
   if (type == "red")
   {#orange: #F07221 #red:cc0000
      return("color: #fff; background-color: #F07221; border-color: #990000; margin-bottom: 2rem; margin-top: 2rem")
   }
   if (type == "green")
   {
      return("color: #fff; background-color: #42C728; border-color: #38A822; margin-bottom: 2rem; margin-top: 2rem")
   }
}


numericInputRow <- function(inputId, label, value = ""){
   div(style="display:inline-block",
       tags$label(label, `for` = inputId), 
       tags$input(id = inputId, type = "text", value = value, class="input-small"))
}

############
### MENU ###
############

menuOutput = function(){
   return(list(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Project settings", tabName = "sett", icon = icon("chevron-down")),
      menuItem("Data", tabName = "data", icon = icon("chevron-down"),
               menuSubItem("Upload", tabName = "dat_upl"), 
               menuSubItem("Filter", tabName = "dat_filter"),
               menuSubItem("View", tabName = "dat_view")),
      menuItem("K-value", tabName = "k_values", icon = icon("chevron-down"),
               menuSubItem("Description", tabName = "k_des"), 
               menuSubItem("Estimation", tabName = "k_est")),
      menuItem("Sap Flow", tabName = "sap_flow", icon = icon("chevron-down"),
               menuSubItem("Description", tabName = "sf_des"), 
               menuSubItem("Sap Flow Index", tabName = "sf_ind"),
               menuSubItem("Sap Flow Density", tabName = "sf_dens"),
               menuSubItem("Sap Flow", tabName = "sf_flow"),
               menuSubItem("Tree Water Use", tabName = "sf_twu")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("th"),
               menuSubItem("VPD", tabName = "vpd"))
      # br(), tags$hr(), br(),
      # downloadButton("DownloadProject", "Download project",
      #                style=buttonStyles())
      ))
}


#############
### ABOUT ###
#############

introOutput = function(){
   return(list(
      fluidRow(
         box(title = "Objective",
             status = "info", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_rm.md")),
         box(title = "Guide",
             status = "warning", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_guide.md"))  
      ),
      fluidRow(
         box(title = "Methods",
             status = "primary", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_meth.md")),
         box(title = "Outputs",
             status = "success", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_out.md"))
      )
   ))   
}


################
### SETTINGS ###
################

settingsOutput = function(){
   return(#list(
      fluidRow(column(8,
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
                 column(4, numericInput("sapWoodDepth", "Sap wood depth (cm)",
                                        value = 0.0)),
                 column(4, numericInput("heartWoodDepth", "Heart wood depth (cm)",
                                        value = 0.0)),
                 column(4, numericInput("barkThickness", "Bark thickness (cm)",
                                        value = 0.0))
              ),
              
              fluidRow(
                 column(6, numericInput("stemCircumference", "Stem circumference (cm)",
                                        value = 0.0)),
                 column(6, numericInput("stemDiameter", "Stem diameter (cm)",
                              value = 0.0))
              ),
              
              numericInput("ThermalDiffusivity", "Thermal diffusivity (cm/s)",
                           value = 0.0025),
              
              h4("Sensor properties"),
              fluidRow(
                 column(6, numericInput("Zax", "Axial sensor distance Zax (mm)",
                                        value = 15)),
                 column(6, numericInput("Ztg", "Tangential sensor distance Ztg (mm)",
                                        value = 5))
              )
          )
      )),
      
      column(4,
         fluidRow(box(title = "Output",
             status = "info", solidHeader = F, #height = 300,
             collapsible = T, width = 12,
             p("These inputs are optional."),
             selectInput("figFor", "Figure format",
                         c(#"svg" = "svg",
                           "pdf" = "pdf",
                           "jpg" = "jpg")),
             checkboxInput("prjNameAsTitle", "Use project name as title", value = T),
             conditionalPanel(
                condition = "input.prjNameAsTitle != true",
                textInput("figTitle", "Figure title", placeholder = "Tree species")
             ),
             p(strong("Visualization")),
             p("NOT WORKING!!!", style = "color:red"),
             
             textInput("gradientColorLow", "Gradient color low", placeholder = "#d8b365"),
             textInput("gradientColorHigh", "Gradient color high", placeholder = "#5ab4ac")
             
             )
      ))
   )
   )
}


############
### DATA ###
############

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
      fluidRow(
        column(4, checkboxInput("positionManual", "Manual position input",
                                F)),
        column(4, checkboxInput("depthManual", "Manual depth input",
                                F)),
        column(4, checkboxInput("distManual", "Manual sensor distance input",
                                F))
      ),
      
      conditionalPanel(
         condition = "input.positionManual == true",
         textInput("positionInput", "Sensor positions",
                   placeholder = "Sensor positions as vector (comma delimited): 1, 2, 3")),

      conditionalPanel(
         condition = "input.depthManual == true",
         textInput("depthInput", "Sensor depths",
                   placeholder = "Sensor depths (cm) as vector (comma delimited): 10, 8, 7.5")),
      
      conditionalPanel(
         condition = "input.distManual == true",
         numericInput("distInput", "Distance between sensors (cm)",
                      value = 1)),
      
      selectInput("sensorType", "Sensor type",
                  choices = c("HFD8-50", "HFD8-100"),
                  selected = "HFD8-100"),

      p("<Note> To calculate sensor depths, information on wood properties are required
        (see 'Project Settings')."),
      
      verbatimTextOutput("depths"),
      
      p("* negative values for 'depth' indicate that the sensor is longer than 
        tree radius (DBH / 2 - barkthickness) and the respective sensor positions are on the opposite side of the tree. "),
      
      
      img(src='stemProfile.png', width = "100%")
      
   ))
}

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
      box(title = "Optional settings",
          collapsible = T,
          status = "info",
          box.dat_upl.depths()#,
          
         )
      )
      
   ))
}

dataFilterOutput = function(){
   return(list(
      fluidRow(
         box(title = "Subset data",
             collapsible = T, width = 4,
             actButton("LoadFilter", "Load filter options", "update"),
             
             uiOutput("filterOptions")
             
         ),
         
         box(title = "Figures",
             collapsible = T, width = 8,
             
             fluidRow(
                column(4, checkboxInput("filterPlot_facetGrid", "Facet wrap", F)),
                column(4, 
                       conditionalPanel(condition = "input.filterPlot_facetGrid == true",
                                        
                                        radioButtons("filterPlot_scales", "Scales", inline = T,
                                                     choices = c("fixed" = "fixed", 
                                                                 "free" = "free")))
                )
             ),
             
             radioButtons("filterPlot_type", "Diagram type", inline = T,
                          choices = c("Boxplot" = "boxp",
                                      "Histogram" = "hist", 
                                      "Frequency polygons" = "freq")),
             
             fluidRow(
                column(4, selectInput("filterPlot_X", "Variable",
                                      choices = c("dTsym.dTas" = "dTsym.dTas",
                                                  "dTas" = "dTas",
                                                  "dTsa" = "dTsa",
                                                  "dTSym" = "dTSym"))),
                column(4, selectInput("filterPlot_col", "Color",
                                      choices = c("none" = "none",
                                                   "doy" = "doy",
                                                  "position" = "position"))),
                column(4, numericInput("filterPlot_binwidth", "Binwidth", value = 0.1))
             ),
             
             output.figure("filterPlot"),
             
         )
      )
   ))
}

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

####################
### K-ESTIMATION ###
####################

kDescriptionOutput <- function(){
   return(list(
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
   ))
}

kValueOutput <- function(){
   return(list(
      box(title = "K-value estimation",
          collapsible = T, #width = 8,
          status = "warning",

          uiOutput("kPositionSelect"),

          tags$hr(),

          selectInput("kMethod", "Method",
                      choices = c("regression" = "regression",
                                  "closest" = "closest",
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

          checkboxInput("dTimeFilter", "Set custom night time", F),
          conditionalPanel(
             condition = "input.dTimeFilter == true",
             fluidRow(
                column(6, numericInput("kRegressionTime.start", label = "Start (0-24 h)",
                                value = 0)),
                column(6, numericInput("kRegressionTime.end", label = "End (0-24 h)",
                                value = 24))
             )
          ),
          
          actButton("setK", "Set k-value", "setValue"),
          
          tabsetPanel(
             
             tabPanel("Selected", br(),
                      br(),
                      output.table("kSelected"),
                      actButton("save.kValues", "Save csv", "saveCsv")),
             tabPanel("Regression", br(),
                      output.table("kRegression")),
             tabPanel("Closest",
                      output.table("kClosest")),
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
                            column(6, numericInput("skip2", "Skip:", min = 0, max = 100, 0)),
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
          
          # radioButtons("k1Plot.scales", "Scales", c("free" = "false", "fixed" = "true"), inline=T),
          radioButtons("k1Plot_scales", "Scales", c("free" = F, "fixed" = T), inline=T),
          
          
          uiOutput("xRangeSlider"),

          tabsetPanel(
             tabPanel("K-diagram", br(),
                      checkboxInput("k1Plot.fullrange", "Fullrange regression", 
                                    value = F),
                      output.figure("kvaluePlot1")),
             tabPanel("Control-diagram 1", br(),
                      checkboxInput("k1Plot.forceOrigin", "Force regression through origin", 
                                    value = F),
                      output.figure("kvaluePlot2")),
             tabPanel("Control-diagram 2", br(),
                      output.figure("kvaluePlot3")),
             tabPanel("Night time", br(),
                      output.figure("kNightTimePlot"))
             ),
          
          actButton("save.kPlots", "Save figures", "saveFigure")
          )
   ))
}

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
                      a("Nadezhdina et al. (2015)", href="https://content.sciendo.com/view/journals/johh/63/2/article-p124.xml",
                        target="_blank"), ",", 
                      HTML('&nbsp;'),
                      a("Nadezhdina (1999)", href="https://academic.oup.com/treephys/article/19/13/885/1623199", target="_blank")),
                    p(strong("Sap flow density:"), 
                      HTML('&nbsp;'),
                      a("Nadezhdina (2018)", href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ",",
                      HTML('&nbsp;'),
                      a("Author", href="", target="_blank")),
                    p(strong("Sap flow:"), 
                      HTML('&nbsp;'),
                      a("Author", href="", target="_blank"), ",",
                      HTML('&nbsp;'),
                      a("Author", href="", target="_blank")),
                    p(strong("Tree water use:"), 
                      HTML('&nbsp;'),
                      a("Author", href="", target="_blank"), ",",
                      HTML('&nbsp;'),
                      a("Author", href="", target="_blank")))
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
   ))   
}


sfDensityOutput <- function(){
   return(list(
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
   ))
}

###################
### DIAGNOSTICS ###
###################

