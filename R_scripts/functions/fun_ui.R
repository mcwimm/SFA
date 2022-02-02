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
      menuItem("Project settings", tabName = "sett", icon = icon("circle-notch")),
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
               menuSubItem("Tree water use", tabName = "sf_flow"))
      ))
}

