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

# treeScaleSimple1 = function(data.depth, sensor.dist){
#    depth = abs(data.depth$depth[1])
#    data.depth$Asd = pi * (depth^2 - (depth - sensor.dist)^2)
#    data.depth$sfdepth = data.depth$SFDsw * data.depth$Asd
#    return(data.depth)
# }


# treeScaleSimple2 = function(data, swd){
#    if (max(abs(data$depth[1]) <= swd)){
#       Asw = pi * max(abs(data$depth[1]))^2
#    } else {
#       Asw = pi * (max(abs(data$depth[1]))^2 - swd^2)
#    }
#    
#    data = data %>% 
#       group_by(datetime) %>% 
#       distinct(menaSfs = mean(SFS), doy, dTime) %>% 
#       mutate(meanSFD = menaSfs /swd) %>% 
#       mutate(sf = meanSFD *Asw)
#    
#    return(data)
# }
# 
# treeScaleSimple3 = function(data.depth){
#    depth = abs(data.depth$depth[1]) -0.5
#    
#    data.depth$Csd = 2 * pi * depth
#    data.depth$sfdepth = data.depth$SFS * data.depth$Csd
#    return(data.depth)
# }


treeScaleSimple1 <- function(data, depths, swd){
   print("In method treeScaleSimple1")
   
   depths = sort(depths, decreasing = T)
   print(depths)
   Aring = do.call(rbind,
                   lapply(2:length(depths), function(x)
                      data.frame(x = depths[x-1], Aring = pi * abs(depths[x]^2 - (depths[x-1])^2))))
   Aring = rbind(Aring, data.frame(x = tail(depths, 1),
                                   Aring = pi * abs(tail(depths, 1)^2 - swd^2)))
   print(Aring)
   data = merge(data, Aring, by.x = "depth", by.y = "x")
   
   
   data$SFdepth = data$SFDsw * data$Aring
   
   data = data %>%
      group_by(datetime) %>%
      mutate(sfM1 = sum(SFdepth) / 1000) %>%
      ungroup()
   return(data)
}

treeScaleSimple2 <- function(data, depths, swd){
   print("In method treeScaleSimple2")
   data = data %>% 
      group_by(datetime) %>% 
      mutate(SFS_mean = mean(SFS), SFD_mean = mean(SFS) / swd) %>% 
      ungroup()
   
   data$SWDarea = pi * (max(depths)^2 - swd^2)
   data$sfM2 = data$SFD_mean * data$SWDarea / 1000
   return(data)
}

treeScaleSimple3 <- function(data, depths, swd){
   print("In method treeScaleSimple3")
   depths = sort(depths, decreasing = T)
   print(depths)
   Cring = do.call(rbind,
                   lapply(2:length(depths), function(x)
                      data.frame(x = depths[x-1], Cring = pi * abs(depths[x] + (depths[x-1])))))
   Cring = rbind(Cring, data.frame(x = tail(depths, 1),
                                   Cring = pi * abs(tail(depths, 1) + swd)))
   print(Cring)
  
   
   data = merge(data, Cring, by.x = "depth", by.y = "x")

   data = data %>% 
      mutate(SFdepthm3 = SFS * Cring) %>% 
      group_by(datetime) %>% 
      mutate(sfM3 = mean(SFdepthm3) / 1000) %>% 
      ungroup()
   return(data)
}

get.sapFlowByMethod <- function(data, method,
                                swd){
                                # ,
                                # sensor.dist){

   print("Method in get.sapFlowByMethod")
   print(method)
   
   # data = merge(data, depths, by = "position")                                 
   depths = unique(data$depth)
   
   if (method == "treeScaleSimple1"){
      print("1")
      
      return(treeScaleSimple1(data, depths, swd))
   }
   
   if (method == "treeScaleSimple2"){
      print("2")
      return(treeScaleSimple2(data, depths, swd))
      
   }
   
   if (method == "treeScaleSimple3"){
      print("3")
      
      return(treeScaleSimple3(data, depths, swd))
      
   }
  
   # if (method == "treeScaleSimple1"){
   #    print("In method 1")
   #    d = do.call(rbind, 
   #                lapply(depths, function(x)
   #                   treeScaleSimple1(data.depth = data[data$depth == x, ], 
   #                                    sensor.dist = sensor.dist)
   #                ))
   #    print(head(d))
   #    newData = d %>% 
   #       group_by(datetime) %>% 
   #       distinct(sf = sum(sfdepth) / 1000, doy = doy, dTime = dTime)
   # }
   # 
   # if (method == "treeScaleSimple2"){
   #    newData = treeScaleSimple2(data, swd) %>% 
   #       distinct(sf = sf / 1000, doy = doy, dTime = dTime)
   # }
   # 
   # if (method == "treeScaleSimple3"){
   #    
   #    d = do.call(rbind, 
   #                lapply(depths, function(x)
   #                   treeScaleSimple3(data.depth = data[data$depth == x, ])
   #                ))
   #    newData = d %>% 
   #       group_by(datetime) %>% 
   #       distinct(sf = mean(sfdepth) / 1000, doy = doy, dTime = dTime) %>% 
   #       ungroup()
   # }
   # return(newData)
}  


# d1=get.sapFlowByMethod(data = data,
#                     method = "treeScaleSimple1",#
#                     swd = 5,
#                     depths = spruce.depths
#                   )
