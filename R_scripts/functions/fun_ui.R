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
   {
      return("color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 2rem")
   }
   if (type == "red")
   {
      return("color: #fff; background-color: #cc0000; border-color: #990000; margin-bottom: 2rem")
   }
   if (type == "green")
   {
      return("color: #fff; background-color: #42C728; border-color: #38A822; margin-bottom: 2rem")
   }
}

############
### MENU ###
############

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
      menuItem("Diagnostics", tabName = "diagnostics",
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


################
### SETTINGS ###
################

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
                         icon = icon("folder-open")),
          actButton("crtPrj", "Create/set project", "create"),
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
                        "jpg" = "jpg")),
          checkboxInput("prjNameAsTitle", "Use project name as title", value = T),
          conditionalPanel(
             condition = "input.prjNameAsTitle != true",
             textInput("figTitle", "Figure title", placeholder = "Tree species")
          )
          )
   ))
}


############
### DATA ###
############

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
          actButton("save_dat_upl", "Save csv", "saveCsv"),
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
          actButton("getTimeRange", "Get time", "update"),
          dateRangeInput("daterange", "Date range:"),
          actButton("updateTime", "Update time", "update")

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
          radioButtons("rawPlot.scales","Scales", c("free" = "false", "fixed" = "true"), inline=T),
          
          # checkboxInput("rawPlot.scales", "Scales free",
          #               value = F),
          br(), tags$hr(), br(),
          actButton("save.deltaTfacetWrap", "Save figure", "saveFigure")),
      box(title = "Temperature differences",
          collapsible = T, width = 8,
          status = "info",
          output.figure("deltaTfacetWrap"))
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
          includeMarkdown("./man/des_k_diagrams.md"))
   ))
}

kValueOutput <- function(){
   return(list(
      box(title = "Settings",
          collapsible = T, #width = 8,
          status = "warning",
          sliderInput("kDepthSelect", "Depth", value = 1, 
                      min = 1, max = 10, step = 1),
          radioButtons("k1Plot.scales","Scales", c("free" = "false", "fixed" = "true"), inline=T),
          sliderInput("k1Plot.x", "x-range",
                      min = -10, max = 10, step = 0.25,
                      value = c(-2, 2)),
          checkboxInput("k1Plot.fullrange", "Fullrange regression", 
                        value = F),
          
          tags$hr(), tags$hr(),
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
          actButton("kCreate", "Create new selection", "create"),
          actButton("setK", "Set k-value", "setValue")
          ),
      box(title = "Figures",
          collapsible = T,
          status = "info",
          actButton("save.kPlots", "Save figures", "saveFigure"),
          tabsetPanel(
             tabPanel("Plot 1", br(),
                      output.figure("kvaluePlot1")),
             tabPanel("Plot 2", br(),
                      output.figure("kvaluePlot2")),
             tabPanel("Plot 3", br(),
                      output.figure("kvaluePlot3"))
             )
          ),
      box(title = "K-values",
          collapsible = T,
          status = "info",
          tabsetPanel(
             
             tabPanel("Selected", br(),
                      br(),
                      actButton("save.kValues", "Save csv", "saveCsv"),
                      output.table("kSelected")),
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
                         
                         numericInput("skip2", "Skip:", min = 0, max = 100, 0),
                         checkboxInput("header2", "Header", TRUE),
                         radioButtons("sep2", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t"),
                                      selected = ",")),
                      output.table("uploadedKvalues"))
             )
          
          )
   ))
}

################
### SAP FLOW ###
################

sdDescriptionOutput <- function(){
   return(list(
      box(title = "Sap Flow Index",
          collapsible = T, status = "info",
          includeMarkdown("./man/des_sf_index.md")),
      box(title = "Sap Flow Density",
          collapsible = T, status = "info",
          includeMarkdown("./man/des_sf_density.md")),
      box(title = "Sap Flow",
          collapsible = T, status = "info",
          includeMarkdown("./man/des_sf.md")),
      box(title = "Tree water use",
          collapsible = T, status = "info",
          includeMarkdown("./man/des_sf_twu.md")),
      box(title = "References",
          collapsible = T, status = "info",
          p(strong("Sap flow index:"), 
            HTML('&nbsp;'),
            a("Nadezhdina et al. (2015)", href="https://content.sciendo.com/view/journals/johh/63/2/article-p124.xml",
              target="_blank"), ",", 
            HTML('&nbsp;'),
            a("Nadezhdina (1999)", href="https://academic.oup.com/treephys/article/19/13/885/1623199", target="_blank")),
          p(strong("Sap flow density:"), 
            HTML('&nbsp;'),
            a("Nadezhdina (2018", href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ",",
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
      
   ))
}

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
          checkboxInput("sfIndexPlot.wrap", "Facet wrap",
                        value = T),
          br(), tags$hr(), br(),
          actButton("save.sfIndex", "Save figure `Complete`", "saveFigure"),
          actButton("save.sfIndex.day", "Save figure `Daily`", "saveFigure")),
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




###################
### DIAGNOSTICS ###
###################

