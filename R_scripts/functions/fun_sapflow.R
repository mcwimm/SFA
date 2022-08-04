#' Sapwood depth
#' @description Estimates sapwood depth based on ui-inputs
#' @param ui.input: UI-input
#' @return numeric
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

#' Sap flow per section and density
#' Calculated with positive or negative formula (see Nadezhdina & Nadezhdin, 2017)
#' @param method: character
#' @param data: data.frame, long-format
#' @param sapWoodDepth: numeric
#' @param ui.input: UI-input
#' @return data.frame
get.sapFlowDensity <- function(method = "HFD", data, 
                               sapWoodDepth, ui.input){
   data$Dst = ui.input$ThermalDiffusivity
   data$Zax = ui.input$Zax
   data$Ztg = ui.input$Ztg
   
   if (method == "HFD"){
      # Positive SFS is always stored for further processing
      data$SFSpos = 3600 * data$Dst * (data[, "k"] + data[, "dTsa"]) / data[, "dTas"] *
         data$Zax / data$Ztg
      # Eq. 1 in NNadezhdina & Nadezhdin, 2017
      if (ui.input$sf_formula == "Positive"){
         data$SFS = data$SFSpos
      }
      # Eq. 2 in Nadezhdina & Nadezhdin, 2017
      if (ui.input$sf_formula == "Negative"){
         data = get.negativeSFS(data, ui.input)
      }
      if (sapWoodDepth != 0){
         data$SFDsw = NA
         data$SFDsw = data$SFS / sapWoodDepth
      }
   }
   return(data)
}


#' Helpfer function to calculate sap flow per section with negative formula
#' 3 options are available: default threshold, i.e. 0°C, manual defined threshold
#' applied to all thermometer positions, individual manual threshold for each
#' positions
#' @param data: data.frame, long-format
#' @param ui.input: UI-input
#' @return data.frame
get.negativeSFS = function(data, ui.input){
   # Use default threshold of 0°C
   if (ui.input$sf_formula_threshold == ""){
      print("Apply default threshold")
      data = data %>%
         mutate(neg_SFS_threshold = 0,
                SFS = ifelse(dTSym >= 0, 3600 * Dst * (k + dTsa) /
                                dTas * Zax / Ztg,
                             -3600 * Dst * (-k + dTas) / dTsa * Zax / Ztg))
   } else {
      # Use manual threshold
      
      # remove white spaces
      sf_formula_threshold = gsub(" ", "", ui.input$sf_formula_threshold, fixed = TRUE)
      # get individual values
      sf_formula_threshold = strsplit(sf_formula_threshold, ",")[[1]]

      no_thresholds = length(sf_formula_threshold)
      print(paste("Number of threshods ", no_thresholds))
      # Apply threshold to all positions
      if (no_thresholds == 1){
         threshold = as.numeric(sf_formula_threshold)
         print(paste("Apply threshold to all positions: ", threshold))
         data = data %>%
            mutate(neg_SFS_threshold = threshold,
                   SFS = ifelse(dTSym >= threshold, 3600 * Dst * (k + dTsa) /
                                   dTas * Zax / Ztg,
                                -3600 * Dst * (-k + dTas) / dTsa * Zax / Ztg))
      } else {
         no_positions = length(unique(data$position))

         sf_formula_thresholds = as.numeric(sf_formula_threshold)

         new_data = data.frame()
         positions = sort(unique(data$position))
         for (i in 1:no_thresholds){
            pos = positions[i]
            threshold = as.numeric(sf_formula_threshold[i])
            print(paste("Apply threshold ", threshold, " to position ", pos, "."))
            
            d = data %>%
               filter(position == pos) %>% 
               mutate(neg_SFS_threshold = threshold,
                      SFS = ifelse(dTSym >= threshold, 3600 * Dst * (k + dTsa) /
                                      dTas * Zax / Ztg,
                                   -3600 * Dst * (-k + dTas) / dTsa * Zax / Ztg))
            new_data = rbind(new_data, d)

         }
         data = new_data
         
      }
   }
   return(data)
}



#### Scale tree-level ####

#' Upscaling method 1: area of circular rings
#' @param data: data.frame, long-format
#' @param sapWoodDepth: numeric
#' @return data.frame
treeScaleSimple1 <- function(data, swd) {
   # Calculate sap flow rate at each sensor depth
   data$SFdepth = data$SFDsw * data$Aring
   
   # Calculate sap flow rate per time step over alls depths in kg/h
   data = data %>%
      group_by(datetime) %>%
      mutate(sfM1 = sum(SFdepth, na.rm = T) / 1000) %>%
      ungroup()
   return(data)
}

#' Upscaling method 2: sapwood area
#' @param data: data.frame, long-format
#' @param sapWoodDepth: numeric
#' @return data.frame
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

#' Upscaling method 2: circumference of circular rings
#' @param data: data.frame, long-format
#' @param sapWoodDepth: numeric
#' @return data.frame
treeScaleSimple3 <- function(data, swd) {
   data$SFdepthm3 = data$SFS * data$Cring
   
   data = data %>%
      group_by(datetime) %>%
      mutate(sfM3 = mean(SFdepthm3, na.rm = T) / 1000) %>%
      ungroup()
   return(data)
}

#' Sap flow by method (helper)
#' @param data: data.frame, long-format
#' @param sapWoodDepth: numeric
#' @param method: character
#' @return data.frame
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

#' Sap flow with different methods
#' @param data: data.frame, long-format
#' @param depths: vector
#' @param sapWoodDepth: numeric
#' @param ui.input: UI-input
#' @return data.frame
get.sapFlow <- function(data, depths, sapWoodDepth, ui.input){
   methods <- list("treeScaleSimple1" = ui.input$treeScaleSimple1,
                   "treeScaleSimple2" = ui.input$treeScaleSimple2,
                   "treeScaleSimple3" = ui.input$treeScaleSimple3)
   if (ui.input$inputType == "HFD_processed_read"){
      data = data
   } else {
      # remove columns if in write mode
      if(ui.input$inputType == "HFD_processed_write"){
         data[, c("depth", "Aring", "R", "Cring")] = NULL
      }
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
   }
   
   return(data)
}

#' Tree water use by method (helper)
#' @param data: data.frame, long-format
#' @param ui.input: UI-input
#' @return data.frame
get.treeWaterUseByMethod = function(data, ui.input){
   groups = c()
   if (ui.input$treeScaleSimple1){
      groups = rbind(groups, "sfM1")
   }
   if (ui.input$treeScaleSimple2){
      groups = rbind(groups, "sfM2")
   }
   if (ui.input$treeScaleSimple3){
      groups = rbind(groups, "sfM3")
   }
   
   groups = groups[,1]
   
   data = data %>% 
      gather(., Method, SFrate, groups) %>% 
      mutate(Method = ifelse(Method == "sfM1", "Method 1",
                             ifelse(Method == "sfM2", "Method 2",
                                    "Method 3")),
             Balance = ifelse(SFrate >= 0, "Positive", "Negative")) %>% 
      mutate(Balance = factor(Balance, levels = c("Positive", "Negative"))) %>% 
      filter(complete.cases(.)) %>% 
      # distinct(datetime, doy, dTime, Method, SFrate, Balance) %>%
      select(datetime, doy, dTime, Method, SFrate, Balance) %>% 
      unique(.) %>% 
      group_by(doy, Method, Balance) %>% 
      arrange(dTime) %>% 
      mutate(roll_mean = (SFrate + lag(SFrate))/2,
             delta_x = dTime - lag(dTime),
             trapezoid = delta_x * roll_mean) %>% 
      # distinct(auc = sum(trapezoid, na.rm = T)) %>%
      mutate(auc = sum(trapezoid, na.rm = T)) %>%
      select(doy, Method, Balance, auc) %>% 
      unique(.) %>% 
      # Credits for AUC below: Victor Klos (https://stackoverflow.com/a/30280873)
      # distinct(auc = sum(diff(dTime) * (head(SFrate,-1)+tail(SFrate,-1)))/2) %>%  
      rename('tree water use' = 'auc',
             "Day of year" = "doy") %>%   ungroup() %>% 
      spread(., Balance, 'tree water use') %>% 
      mutate_if(is.numeric, round, 2)

   return(data)
}

