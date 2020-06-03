######## GENERAL ###########

output.table = function(outputID){
   return(list(br(), 
          DT::dataTableOutput(outputID) %>% 
             withSpinner(color="#0dc5c1")))
}

output.figure = function(outputID){
   return(plotOutput(outputID) %>% withSpinner(color="#0dc5c1"))
}

######## MENU ###############

menuOutput = function(){
   return(list(
      menuItem("About", tabName = "about"),
      menuItem("Project settings", tabName = "sett"),
      menuItem("Data", tabName = "data",
               menuSubItem("Upload", tabName = "dat_upl"), 
               menuSubItem("View", tabName = "dat_view")),
      menuItem("K-value", tabName = "k_values",
               menuSubItem("Description", tabName = "k_des"), 
               menuSubItem("Estimation", tabName = "k_est")),
      menuItem("Sap Flow", tabName = "sap_flow",
               menuSubItem("Description", tabName = "sf_des"), 
               menuSubItem("Sap Flow Index", tabName = "sf_ind"),
               menuSubItem("Sap Flow Density", tabName = "sf_dens"),
               menuSubItem("Sap Flow", tabName = "sf_flow"),
               menuSubItem("Tree Water Use", tabName = "sf_twu")),
      br(), tags$hr(), br(),
      downloadButton("DownloadProject", "Download project",
                     style=buttonStyles())
      ))
}

############################ 
######### BOXES ############
############################ 

######### About ############

introOutput = function(){
   return(list(
      box(title = "Objective",
          status = "info", solidHeader = F, #height = 300,
          collapsible = T,
          includeMarkdown("./man/des_main_rm.md")),
      box(title = "Functions",
          status = "warning", solidHeader = F, #height = 300,
          collapsible = T,
          includeMarkdown("./man/des_main_data.md")),
      box(title = "Methods",
          status = "primary", solidHeader = F, #height = 300,
          collapsible = T,
          includeMarkdown("./man/des_main_meth.md")),
      box(title = "Outputs",
          status = "success", solidHeader = F, #height = 300,
          collapsible = T,
          includeMarkdown("./man/des_main_out.md"))
   ))   
}

######### Settings #########

settingsOutput = function(){
   return(list(
      box(title = "Project",
          status = "warning", solidHeader = F, #height = 300,
          collapsible = T,
          
          p("Please select a current project or create a new folder:"),
          
          shinyDirButton('folder', 
                         'Folder select', 
                         'Please select a folder', 
                         # multiple = FALSE,
                         style = buttonStyles("red"),
                         icon("folder-open")),
          actionButton("crtPrj", "Create/set project",
                       style = buttonStyles("blue"),
                       icon("broom")),
          br(), br(),
          h4("Current project"),
          verbatimTextOutput("prjName"),
          h4("Current project directory"),
          verbatimTextOutput("prjDir")
      ),
      box(title = "Wood properties",
          status = "warning",
          collapsible = T,
          numericInput("stemCircumference", "Stem circumference (cm)",
                       value = 0.0),
          numericInput("stemDiameter", "Stem diameter (cm)",
                       value = 0.0),
          # numericInput("barkThickness", "Bark thickness (cm)"),
          numericInput("sapWoodDepth", "Sap wood depth (cm)",
                       value = 0.0)),
      box(title = "Output",
          status = "info", solidHeader = F, #height = 300,
          collapsible = T,
          p("These inputs are optional."),
          selectInput("figFor", "Figure format",
                      c("svg" = "svg",
                        "pdf" = "pdf",
                        "jpg" = "jpg")))
   ))
}

######### Data #############

box.dat_upl.upload = function(){
   return(list(
      br(),
      selectInput("inputType", "Input file type/ manufacturer",
                  c("ICT_raw" = "ICT_raw", 
                    "ICT_delta" = "ICT_delta")),
      br(), 
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = F,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      numericInput("skip", "Skip:", min = 0, max = 100, 10),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
   ))
}

box.dat_upl.depths = function(){
   return(list(
      checkboxInput("depthManual", "Manual input",
                    F),
      conditionalPanel(
         condition = "input.depthManual == true",
         textInput("depthInput", "Sensor depths",
                   placeholder = "Sensor depths as vector (comma delimited): 1, 2, 3")),
      h5('Extracted sensor depths'),
      verbatimTextOutput("depths")
   ))
}

dataUplOutput = function(){
   return(list(
      box(title = "Description",
          collapsible = T,
          status = "info",
          includeMarkdown("./man/des_data.md"), br(),
          tags$hr(),
          # downloadButton("save_dat_upl", "Save csv"),
          actionButton("save_dat_upl", "Save csv",
                       icon("file-download"),
                       style=buttonStyles()),
          br()),
      box(title = "Upload file",
          collapsible = T,
          status = "warning",
          box.dat_upl.upload()),
      box(title = "Data",
          collapsible = T,
          status = "info",
          
         tabsetPanel(
            tabPanel("wide", br(),
                     output.table("raw.wide")),
            tabPanel("long", br(),
                     output.table("raw.long")))),
      box(title = "Other",
          collapsible = T,
          status = "info",
          h4("Sensor depths"),
          box.dat_upl.depths(),
          tags$hr(),
          h4("Subset data"),
          p("NOT WORKING!!!", style = "color:red"),
          actionButton("getTimeRange", "Get time",
                       icon("broom")),
          dateRangeInput("daterange", "Date range:"),
          actionButton("updateTime", "Update time",
                       icon("broom"))
          
      )
      
   ))
}

dataViewOutput = function(){
   return(list(
      box(title = "Settings",
          collapsible = T, width = 4,
          status = "warning",
          sliderInput("rawPlot.x", "x-axis range",
                      min = -10, max = 10, step = 0.25,
                      value = c(-0.5, 1.5)),
          sliderInput("rawPlot.y", "y-axis range",
                      min = -10, max = 10, step = 0.25,
                      value = c(-2, 2)),
          checkboxInput("rawPlot.scales", "Scales free",
                        value = F),
          br(), tags$hr(), br(),
          actionButton("save.deltaTfacetWrap",
                       "Save figure", icon("chart-bar"),
                       style=buttonStyles())),
      box(title = "Temperature differences",
          collapsible = T, width = 8,
          status = "info",
          output.figure("deltaTfacetWrap"))
   ))   
}

######### K-value estimation ###

kValueOutput <- function(){
   return(list(
      box(title = "Settings",
          collapsible = T, #width = 8,
          status = "warning",
          sliderInput("kDepthSelect", "Depth", value = 1, 
                      min = 1, max = 10, step = 1),
          sliderInput("k1Plot.x", "x-range",
                      min = -10, max = 10, step = 0.25,
                      value = c(-2, 2)),
          checkboxInput("k1Plot.fullrange", "Fullrange regression", 
                        value = F),
          
          tags$hr(), tags$hr(),
          selectInput("kMethod", "Method",
                      choices = c("manual" = "manual",
                                  "closest" = "closest",
                                  "regression" = "regression")),
          actionButton("setK", "Set k-value",
                       style = buttonStyles("red"),
                       icon("check-circle")),
          actionButton("save.kValues", "Save csv",
                       style = buttonStyles("blue"),
                       icon("check-circle"))
          ),
      box(title = "Figures",
          collapsible = T,
          status = "info",
          tabsetPanel(
             tabPanel("Plot1", br(),
                      output.figure("kvaluePlot1")),
             tabPanel("Plot2", br(),
                      # output.figure("kvaluePlot2"))
             ))
          ),
      box(title = "K-values",
          collapsible = T,
          status = "info",
          tabsetPanel(
             tabPanel("Automatic", br(),
                      actionButton("update.Kvalues", "Update list",
                                   style = buttonStyles("green"),
                                   icon("broom")),  
                      output.table("existingKvalues")),
             tabPanel("Manual", br(),
                      uiOutput("manKvalues"),
                      actionButton("save.manK", "Save values",
                                   style = buttonStyles("blue"),
                                   icon("check-circle"))),
                      # output.figure("kvaluePlot2"))
             tabPanel("Closest",
                      output.table("kClosest")),
             tabPanel("Read csv", br(),
                      fileInput("file2", "Choose CSV File",
                                multiple = F,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),
                      output.table("uploadedKvalues"))
             )
          
          )
   ))
}

######### Sap Flow Index ###

sfIndexOutput <- function(){
   return(list(
      box(title = "Settings",
          collapsible = T, width = 4,
          status = "warning",
          sliderInput("sfIndexPlot.x", "x-axis range",
                      min = 0, max = 24, step = 0.25,
                      value = c(0, 24)),
          sliderInput("sfIndexPlot.y", "y-axis range",
                      min = -10, max = 10, step = 0.25,
                      value = c(-0.5, 2)),
          checkboxInput("sfIndexPlot.scales", "Scales free",
                        value = F),
          br(), tags$hr(), br(),
          actionButton("save.sfIndex",
                       "Save figure `Complete`", icon("chart-bar"),
                       style=buttonStyles()),
          actionButton("save.sfIndex.day",
                       "Save figure `Daily`", icon("chart-bar"),
                       style=buttonStyles())),
      box(title = "Sap Flow Index",
          collapsible = T, width = 8,
          status = "info",
          tabsetPanel(
             tabPanel("Complete", br(),
                      output.figure("sapFlowIndex")),
             tabPanel("Daily", br(),
                      output.figure("sapFlowIndex.Day"))
          ))
   ))   
}



######## STYLE ############

buttonStyles = function(type = "blue"){
   if (type == "blue")
   {
      return("color: #fff; background-color: #337ab7; border-color: #2e6da4")
   }
   if (type == "red")
   {
      return("color: #fff; background-color: #cc0000; border-color: #990000")
   }
   if (type == "green")
   {
      return("color: #fff; background-color: #42C728; border-color: #38A822")
   }
}



