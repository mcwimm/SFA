get.sapWoodDepth = function(ui.input){
   if (ui.input$sapWoodDepth == 0){
      if (ui.input$stemCircumference == 0){
         swd = ui.input$stemDiameter/2 - ui.input$barkThickness - ui.input$heartWoodDepth
      } else {
         swd = ui.input$stemCircumference / (2*pi) - ui.input$barkThickness -
            ui.input$heartWoodDepth
      }} else {
         swd = ui.input$sapWoodDepth
      }
   return(swd)
}

add.k2data = function(data, values){
   # Get sensor positions for which a k-value exist
   kValues = values$kvalues
   kValues[, "k"] = as.numeric(kValues[, "k"])
   positions = unique(kValues[!is.na(kValues$k), ]$position)
   
   # Filter data by positions
   data = data %>% 
      filter(position %in% positions)
   
   # Add k-value to data set by position
   data = merge(data, kValues[, c("position", "k")], by = "position")
   data = data[!is.na(data$datetime), ]
   return(data)
}

get.sapFlowDensity <- function(method = "HFD", data, 
                               sapWoodDepth, ui.input){
   Dst = ui.input$ThermalDiffusivity
   Zax = ui.input$Zax
   Ztg = ui.input$Ztg
   
   if (method == "HFD") {
      data$SFS = 3600 * Dst * (data[, "k"] + data[, "dTsa"]) / data[, "dTas"] *
         Zax / Ztg
      data$SFDsw = NA
      data$SFDsw = data$SFS / sapWoodDepth
      
   }
   return(data)
}


get.sensorDistance <- function(ui.input){
   if (ui.input$sensorType == "HFD8-50"){
      return(0.5)
   }
   if (ui.input$sensorType == "HFD8-100"){
      return(1)
   }
   if (ui.input$sensorType == "Manual"){
      return(ui.input$distInput)
   }
}


#### Scale tree-level ####

treeScaleSimple1 <- function(data, swd) {
   # Calculate sap flow rate at each sensor depth
   data$SFdepth = data$SFDsw * data$Aring
   
   # Calculate sap flow rate per time step over alls depths in kg/h
   data = data %>%
      group_by(datetime) %>%
      mutate(sfM1 = sum(SFdepth) / 1000) %>%
      ungroup()
   return(data)
}

treeScaleSimple2 <- function(data, swd) {
   depths = unique(data$depth)
   
   # Calculate mean sap flow per section and divide it by sap wood depth
   data = data %>%
      group_by(datetime) %>%
      mutate(SFS_mean = mean(SFS, na.rm = T),
             SFD_mean = mean(SFS_mean, na.rm = T) / swd) %>% 
      ungroup()
   
   # Calculate sap wood area, which is the difference in total stem area (A_rxy) and
   # heart wood area
   A_rxy = pi * max(abs(depths))^2
   A_hw = pi * (max(abs(depths)) - swd)^2

   data$SWDarea = A_rxy - A_hw

   # Calculate sap flow rate per time step over alls depths in kg/h
   data$sfM2 = data$SFD_mean * data$SWDarea / 1000
   return(data)
}

treeScaleSimple3 <- function(data, swd) {
   data$SFdepthm3 = data$SFS * data$Cring
   
   data = data %>%
      group_by(datetime) %>%
      mutate(sfM3 = mean(SFdepthm3, na.rm = T) / 1000) %>%
      ungroup()
   return(data)
}

get.sapFlowByMethod <- function(data, method, swd) {
   if (method == "treeScaleSimple1") {
      return(treeScaleSimple1(data, swd))
   }
   
   if (method == "treeScaleSimple2") {
      return(treeScaleSimple2(data, swd))
   }
   
   if (method == "treeScaleSimple3") {
      return(treeScaleSimple3(data, swd))
   }
}

get.sapFlow <- function(data, depths, sapWoodDepth, ui.input){
   methods <- list("treeScaleSimple1" = ui.input$treeScaleSimple1,
                   "treeScaleSimple2" = ui.input$treeScaleSimple2,
                   "treeScaleSimple3" = ui.input$treeScaleSimple3)
   
   data = data %>% 
      merge(., depths, by = "position")
   
   for (m in c(1:length(methods))){
      if (methods[[m]]){
         method = names(methods)[m]
         data = get.sapFlowByMethod(data = data,
                                    method = method, 
                                    swd = sapWoodDepth) 
         
      } 
   }        
   return(data)
}

get.treeWaterUseByMethod = function(data, input){
   groups = c()
   if (input$treeScaleSimple1){
      groups = rbind(groups, "sfM1")
   }
   if (input$treeScaleSimple2){
      groups = rbind(groups, "sfM2")
   }
   if (input$treeScaleSimple3){
      groups = rbind(groups, "sfM3")
   }
   
   groups = groups[,1]
   
   # Credits for AUC: Victor Klos (https://stackoverflow.com/a/30280873)
   data = data %>% 
      gather(., Method, SFrate, groups) %>% 
      mutate(Method = ifelse(Method == "sfM1", "Method 1",
                             ifelse(Method == "sfM2", "Method 2",
                                    "Method 3")),
             Balance = ifelse(SFrate >= 0, "Positive", "Negative")) %>% 
      mutate(Balance = factor(Balance, levels = c("Positive", "Negative"))) %>% 
      filter(complete.cases(.)) %>% 
      group_by(doy, Method, Balance) %>% 
      arrange(dTime) %>% 
      distinct(auc = sum(diff(dTime) * (head(SFrate,-1)+tail(SFrate,-1)))/2) %>% 
      rename('tree water use' = 'auc',
             "Day of year" = "doy") %>% 
      spread(., Balance, 'tree water use') %>% 
      mutate_at(c(3,4), round, 2)
   
   return(data)
}

