get.sapFlowDensity <- function(method = "HFD", data, Dst, Zax, Ztg,
                               sapWoodDepth) {
   if (method == "HFD") {
      data$SFS = 3600 * Dst * (data[, "k"] + data[, "dTsa"]) / data[, "dTas"] *
         Zax / Ztg
      data$SFDsw = NA
      data$SFDsw = data$SFS / sapWoodDepth
      
   }
   return(data)
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
   A_rxy = pi * max(depths)^2
   A_hw = pi * (max(depths) - swd)^2
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

get.sapFlowByMethod <- function(data, method,
                                swd) {
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
   print(paste('groups  ', groups))
   return(data %>% 
      gather(., key, value, groups) %>% 
      group_by(doy, key) %>% 
      mutate(key = ifelse(key == "sfM1", "method 1",
                          ifelse(key == "sfM2", "method 2",
                                 "method 3"))) %>% 
      arrange(dTime) %>% 
      mutate(delta_time = dTime - lag(dTime),
             delta_sp = abs(value - lag(value))) %>% 
      mutate(m = delta_time * delta_sp) %>% 
      distinct(s_kg_h = sum(m, na.rm = T),
               s_kg_d = round(s_kg_h*24, 2)) %>%
      arrange(doy) %>% 
      select(-s_kg_h) %>% 
      spread(., key, s_kg_d))
}

