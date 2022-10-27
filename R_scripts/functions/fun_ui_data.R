############
### DATA ###
############

### Upload ###
dataUplOutput = function(){
   return(
      fluidRow(
         column(6,
                box(title = "Upload file",
                    collapsible = T, width = "100%",
                    status = "warning",
                    box.dat_upl.upload()
                    ),
                box(title = "Description", collapsed = T,
                    collapsible = T, width = "100%",
                    status = "info",
                    includeMarkdown("./man/des_data.md"))
                ),
         column(6,
                box(title = "Preview data",
                    collapsible = T, width = "100%",
                    status = "success",
                    
                    tabsetPanel(
                       tabPanel("wide", br(),
                                output.table("raw.wide")),
                       tabPanel("long", br(),
                                actButton("save_dat_upl", "Save file", "saveCsv"),
                                br(),
                                output.table("raw.long")))))
      ))
}

box.dat_upl.upload = function(){
   return(list(
      # Input: Select a file ----
      fluidRow(
         column(6, fileInput("file1", "Choose CSV File",
                             multiple = F,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))),
         tags$style("
             .btn-file {  
             background-color: #78875D; 
             border-color: #404731; 
             color: #fff;
             }

             .progress-bar {
             background-color: #78875D;
             }

             "),
         column(6,  selectInput("inputType", "Input file type",
                     c("Raw" = "HFD_raw", 
                       "Delta" = "HFD_delta",
                       "Processed read" = "HFD_processed_read",
                       "Processed write" = "HFD_processed_write")))
      ),
      fluidRow(
         column(6,  selectInput("sep", "Separator", 
                                choices = c("Semicolon" = ";",
                                            "Comma" = ",",
                                            "Tab" = "\t"))),
         column(6,   numericInput("skip", "Skip:", min = 0, max = 100, 10))        
      ),
      fluidRow(
         column(6, actButton("setData", "Use data", "create")),
         # Invisible text output to get file name in header
         column(6, span(textOutput("fileName"), style="color:white"))
      ),
      p(em("<Note> If your data set contains non-numeric rows, e.g. logger warnings,
           they are converted to NA values. Remove them in the 'Data > Filter' section."))
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
                    status = "success",
                    box.filter.figures(),
                    actButton("save_dat_filter", "Save file", "saveCsv"),
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
                                 "Facet grid (position ~ date)", F))
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
                                           "dTsym" = "dTSym"))),
         column(4, selectInput("filterPlot_col", "Color/ Group",
                               choices = c("date" = "date",
                                           "position" = "position",
                                           "none" = "none"))),
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
                                              "day time" = "dTime",
                                              "date-time" = "datetime"))),
             column(6, selectInput("rawPlot.ycol", "Y-axis",
                                  choices = c("dTas" = "dTas",
                                              "dTsa" = "dTsa",
                                              "dTSym" = "dTSym",
                                              "dTsym.dTas" = "dTsym.dTas")))
          ),
          fluidRow(
             column(6, selectInput("rawPlot.col", "Color",
                                   choices = c("day time" = "dTime",
                                               "date" = "date",
                                               "doy" = "doy",
                                               "position" = "position",
                                               "none" = "none"))),
             column(6, selectInput("rawPlot.shape", "Shape/ group",
                                   choices = c("position" = "position",
                                               "date" = "date",
                                               "doy" = "doy",
                                               "none" = "none")))
          ),
          
          checkboxInput("rawPlot_facetWrap", "Facet wrap", F),
          
          conditionalPanel(
             condition = "input.rawPlot_facetWrap == true",
             radioButtons("rawPlot_scales","Scales", 
                          choiceNames =  list(
                             "fixed", "free", "free x", "free y"
                          ),
                          choiceValues = list("fixed", "free", "free_x", "free_y"),
                          inline=T),
             fluidRow(
                column(6, selectInput("rawPlot.facet", "Facet",
                                      choices = c("position" = "position",
                                                  "date" = "date",
                                                  
                                                  "doy" = "doy"))),
                column(6, numericInput("rawPlot.columns", "No. columns",
                                       value = 3))
             )
          ),
          # tags$hr(),
          checkboxInput("rawPlot_lines", "Draw lines instead of points", F),
          checkboxInput("rawPlot_zerolines", "Show zero-temperature line(s) (x=y=0)", F),
          
          tags$hr(),
          checkboxInput("rawPlot_gathered", "Plot all temperature differences over time", F),
          
          actButton("renderPlot", "Render figure", "update")
      
      ),
      box(title = "Figure",
          collapsible = T, width = 8,
          status = "success",
          output.figure("custumPlot"),
          actButton("save.custumPlot", "Save figure", "saveFigure"))
   ))   
}

