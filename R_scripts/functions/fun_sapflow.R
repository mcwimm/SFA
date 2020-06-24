get.sapFlowDensity <- function(method = "HFD", data, Dst, Zax, Ztg,
                               sapWoodDepth){
   if (method == "HFD"){
      data$SFS = 3600 * Dst * (data[, "k"] + data[, "dTsa"]) / data[, "dTas"] *
         Zax / Ztg
      data$SFDsw = NA
      data$SFDsw = data$SFS / sapWoodDepth
      
   }
   return(data)
}


#### Scale tree-level ####

treeScaleSimple1 = function(data.depth, sensor.dist){
   depth = data.depth$depth[1]
   data.depth$Asd = pi * (depth^2 - (depth - sensor.dist)^2)
   data.depth$sfdepth = data.depth$SFDsw * data.depth$Asd
   return(data.depth)
}


treeScaleSimple2 = function(data, swd){
   Asw = pi * (max(data$depth[1])^2 - swd^2)
   
   data = data %>% 
      group_by(datetime) %>% 
      distinct(menaSfs = mean(SFS), doy, dTime) %>% 
      mutate(meanSFD = menaSfs /swd) %>% 
      mutate(sf = meanSFD *Asw)
   
   return(data)
}

treeScaleSimple3 = function(data.depth){
   depth = data.depth$depth[1]
   
   data.depth$Csd = 2 * pi * depth
   data.depth$sfdepth = data.depth$SFS * data.depth$Csd
   return(data.depth)
}


get.sapFlowByMethod <- function(data, method,
                                swd,
                                sensor.dist = 10){
   print("Head data in get.sapFlowByMethod")
   print(head(data))
   print("Method in get.sapFlowByMethod")
   print(method)
   
   depths = unique(data$depth)
   
   if (method == "treeScaleSimple1"){
      
      d = do.call(rbind, 
                  lapply(depths, function(x)
                     treeScaleSimple1(data.depth = data[data$depth == x, ], 
                                      sensor.dist = sensor.dist)
                  ))
      newData = d %>% 
         group_by(datetime) %>% 
         distinct(sf = sum(sfdepth) / 1000, doy = doy, dTime = dTime)
   }
   
   if (method == "treeScaleSimple2"){
      newData = treeScaleSimple2(data, swd) %>% 
         distinct(sf = sf / 1000, doy = doy, dTime = dTime)
   }
   
   if (method == "treeScaleSimple3"){
      
      d = do.call(rbind, 
                  lapply(depths, function(x)
                     treeScaleSimple3(data.depth = data[data$depth == x, ])
                  ))
      newData = d %>% 
         group_by(datetime) %>% 
         distinct(sf = sum(sfdepth) / 1000, doy = doy, dTime = dTime) %>% 
         ungroup()
   }
   return(newData)
}  