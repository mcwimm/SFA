###############
### GENERAL ###
###############

spinner_color = "#b3c88b"

output.table = function(outputID) {
   return(list(
      br(),
      DT::dataTableOutput(outputID) %>%
         withSpinner(color = spinner_color, proxy.height = 150)
   ))
}

output.figure = function(outputID) {
   return(plotOutput(outputID) %>% withSpinner(color = spinner_color, proxy.height = 200))
}

output.html = function(outputID) {
   return(htmlOutput(outputID) %>% withSpinner(color = spinner_color, proxy.height = 50))
}

#############
### STYLE ###
#############

actButton <- function(ID, label, type){
   if (type == "saveCsv"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                   icon("file-download", style="margin-right:.5em")))
   }
   if (type == "saveFigure"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("blue"), "margin-bottom: 2rem", sep = ";"),
                   icon("file-download", style="margin-right:.5em")))
   }
   if (type == "setValue"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("red"), "margin-bottom: 2rem", sep = ";"),
                   icon("check-circle", style="margin-right:.5em")))
   }
   if (type == "create"){
      return(actionButton(ID, label,
                   style = paste(buttonStyles("red"), "margin-bottom: 2rem", sep = ";"),
                   icon("folder-plus", style="margin-right:.5em")))
   }
   if (type == "update"){
      return(actionButton(ID, label,
                   style = buttonStyles("green"), 
                   icon("broom", style="margin-right:.5em")))
   }
   
}

buttonStyles = function(type = "blue"){
   if (type == "blue")
   {
      return("color: #fff; background-color: #78875D; border-color: #404731; margin-bottom: 2rem; margin-top: 2rem")
   }
   if (type == "red")
   {
      return("color: #fff; background-color: #78875D; border-color: #404731; margin-bottom: 2rem; margin-top: 1rem")
   }
   if (type == "green")
   {
      return("color: #fff; background-color: #78875D; border-color: #404731; margin-bottom: 2rem; margin-top: 2rem")
   }
}

buttonStyles2 = function(type = "blue"){
   if (type == "blue")
   {
      return("color: #fff; background-color: #2F8EE0; border-color: #206199; margin-bottom: 2rem; margin-top: 2rem")
   }
   if (type == "red")
   {
      return("color: #fff; background-color: #E04C46; border-color: #99332F; margin-bottom: 2rem; margin-top: 1rem")
   }
   if (type == "green")
   {
      return("color: #fff; background-color: #92B535; border-color: #7C992C; margin-bottom: 2rem; margin-top: 2rem")
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
      menuItem("Settings", tabName = "sett", icon = icon("circle-notch")),
      menuItem("Data", tabName = "data", icon = icon("circle-notch"),
               menuSubItem("Upload", tabName = "dat_upl"), 
               menuSubItem("Filter", tabName = "dat_filter"),
               menuSubItem("View", tabName = "dat_view")),
      menuItem("K-value", tabName = "k_values", icon = icon("circle-notch"),
               menuSubItem("Description", tabName = "k_des"), 
               menuSubItem("Estimation", tabName = "k_est")),
      menuItem("Sap Flow", tabName = "sap_flow", icon = icon("circle-notch"),
               menuSubItem("Description", tabName = "sf_des"), 
               menuSubItem("Sap Flow Metrics", tabName = "sf_metrics"),
               menuSubItem("Tree Water Use", tabName = "sf_flow"))
      ))
}

