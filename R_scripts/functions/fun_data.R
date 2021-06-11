get.rawData = function(input){
   if (input$inputType == "HFD_raw"){
      # print("HFD_raw")
      return(get.temperatures.ICT(input$file1$datapath,
                                  sep = input$sep,
                                  skip = input$skip))
   }
   if (input$inputType == "HFD_delta"){
      # print("HFD_delta")
      return(get.temp.differences.ICT(input$file1$datapath,
                                      sep = input$sep,
                                      skip = input$skip))
   }
}

#' Reads raw temperatures from ICT-data file.
#' 
get.temperatures.ICT = function(file, sep, skip){
   rawData <- read.csv(file,
                       header = TRUE, sep = sep, 
                       fileEncoding="latin1",
                       skip = skip)

   col = grep("Exter", colnames(rawData) )[1]
   rawData = rawData[, c(1:col)]
   
   datetimeformat = get.datetime.format(rawData[1, c("Date", "Time")])
   
   rawData$datetime <- as.POSIXct(x = paste(rawData$Date, rawData$Time), 
                                  format=datetimeformat)
   return(rawData)
}

#' Reads temperature differences from processed ICT-data file.
#' 
get.temp.differences.ICT = function(file, sep, skip){
   rawData <- read.csv(file,
                       header = TRUE, sep = sep, 
                       fileEncoding="latin1",
                       skip = skip)

   if (all(c("Date", "Time") %in% colnames(rawData))){
      datetimeformat = get.datetime.format(rawData[1, c("Date", "Time")])
      rawData$datetime <- as.POSIXct(x = paste(rawData$Date, rawData$Time), 
                                     format=datetimeformat)
   }
   return(rawData)
}

get.datetime.format = function(datetime){
   # datetime = rawData[1, c("Date", "Time")]
   if (!is.na(as.POSIXct(x = paste(datetime$Date, datetime$Time), 
                     format="%d.%m.%Y %H:%M"))){
      # print("%d.%m.%Y %H:%M")
      return("%d.%m.%Y %H:%M")
   }
   if (!is.na(as.POSIXct(x = paste(datetime$Date, datetime$Time), 
                         format="%d/%m/%Y %H:%M"))){
      # print("%d/%m/%Y %H:%M")
      return("%d/%m/%Y %H:%M")
   }
}


#' Symetric and asymetric temperature differences for each needle position.
#' 
#' @description 
#' Returns asymetric and symetric temperature differences as well as 
#' their ratio for each sensor position.
#' 
#' @details 
#' The differences are calculated from raw temperature measurments of 
#' sensors installed above, below and alongside the heater.
#' 
#' @usage get.delta.temp.depth(rawData, position)
#' 
get.delta.from.temp.depth <- function(rawData, position){
   raw_temperatures <- rawData[, grepl("Temp", colnames(rawData))]
   raw_temperatures <- raw_temperatures[, !grepl("Batt", colnames(raw_temperatures))]
   
   reg.U <- paste(position, "U", sep=".")
   reg.L <- paste(position, "L", sep=".")
   reg.S <- paste(position, "S", sep=".")
   
   t_up <- matrix(raw_temperatures[, grepl(reg.U,
                                               colnames(raw_temperatures))])
   
   t_low <- matrix(raw_temperatures[, grepl(reg.L,
                                                colnames(raw_temperatures))])
   
   t_side <- matrix(raw_temperatures[, grepl(reg.S,
                                                 colnames(raw_temperatures))])
   
   if ((ncol(t_up) != 1) | (ncol(t_low) != 1) | (ncol(t_side) != 1)){
      print(paste("There is more than one temperature dataset for position ", position,
            ". Please check your raw data file.", sep = ""))
   }
   
   # delta.temp <- cbind(datetime = rawData[, "datetime"],
   #                     Date = rawData[, "Date"],
   #                     Time = rawData[, "Time"],
   #                       dTas = t_side[, 1] - t_low[, 1],
   #                       dTsa = t_up[, 1] - t_side[, 1],
   #                       dTSym = t_up[, 1] - t_low[, 1])

   delta.temp <- data.frame(
                     datetime = rawData[, "datetime"],
                     dTas = t_side[, 1] - t_low[, 1],
                     dTsa = t_up[, 1] - t_side[, 1],
                     dTSym = t_up[, 1] - t_low[, 1]
                  )
   
   delta.temp = cbind(delta.temp, 
                      "dTsym.dTas" = delta.temp[, "dTSym"] / 
               delta.temp[, "dTas"],
               "timestep" = c(1:nrow(delta.temp)))
   return(delta.temp)
}

#' Symetric and asymetric temperature differences.
#' 
#' @description 
#' Returns asymetric and symetric temperature differences as well as 
#' their ratio.
#' 
get.delta.from.temp = function(rawData, positions){
   delta.temp = do.call(rbind, 
                        lapply(positions, function(x)
      cbind(position = x, 
            get.delta.from.temp.depth(rawData = rawData, position = x))))
   
   # add hour of day and date of year
   delta.temp$dTime = convertTimeToDeci(as.character(rawData$Time))
   delta.temp$doy <- strftime(rawData$datetime, format = "%j")
   
   return(delta.temp)
}


#' Helper function for get.delta.temp
#' 
get.delta.temp.depth = function(rawData, position){
   reg.sym <- paste("dTSym", position, sep = "")
   reg.asym <- paste("dTas", position, sep = "")

   sym <- matrix(rawData[, grepl(reg.sym,
                                           colnames(rawData))])
   
   asym <- matrix(rawData[, grepl(reg.asym,
                                            colnames(rawData))])
   
   if ((ncol(sym) != 1) | (ncol(asym) != 1)){
      print(paste("There is more than one temperature dataset for position ", position,
                  ". Please check your raw data file.", sep = ""))
   }
   delta.temp <- data.frame(
      datetime = rawData[, "datetime"],
      dTas = asym,
      dTSym = sym
   )
   
   delta.temp = cbind(delta.temp, 
                      "dTsa" = delta.temp[, "dTSym"] - 
                         delta.temp[, "dTas"],
                      "dTsym.dTas" = delta.temp[, "dTSym"] / 
                         delta.temp[, "dTas"],
                      "timestep" = c(1:nrow(delta.temp)))
   return(delta.temp)
   
}

#' Make long format delta temperature data from wide format
get.delta.temp = function(rawData, positions){
   delta.temp = do.call(rbind, 
           lapply(positions, function(x)
              cbind(position = x, 
                    get.delta.temp.depth(rawData = rawData, 
                                         position = x))))
   
   delta.temp$dTime = convertTimeToDeci(as.character(rawData$Time))
   delta.temp$doy <- strftime(rawData$datetime, format = "%j")
   return(delta.temp)
}


#' Converts character time HH:MM:SS to decimal time
convertTimeToDeci <- function(time){
   dt = sapply(strsplit(time,":"),
               function(x) {
                  x <- as.numeric(x)
                  x[1]+x[2]/60
               })
   return(dt)
}

#' Sensor positions
#' 
#' @description 
#' Sensor positions are either extracted from raw data file or
#' entered manually
#' 
get.positionsFromRawData = function(dataSource, input){

   if (input$inputType == "HFD_raw"){
      reg = "Asym"
   }
   
   if (input$inputType == "HFD_delta"){
      reg = "dTSym"
   }
   
   if (input$positionManual){
      positions = as.numeric(unlist(strsplit(input$positionInput,",")))
   } else {
      positions = c(1:ncol(dataSource[, grepl(reg, 
                                          colnames(dataSource))]))
   }
   
   return(positions)
}


#' Sensor positions as depth from stem center
#' 
get.depths <- function(depthManual = F, inputType,
                       positions, rxy, depthInput, sensor_distance){
   if (depthManual){
      df = data.frame(position = positions,
                      depth = as.numeric(unlist(strsplit(depthInput, ","))))
   } else {
      # distance between outer stem ring and first sensor is 2 cm
      if (inputType == "HFD8-50"){ # sensorLength = 6.2
         df = data.frame(position = c(1:8),
                         depth = seq((rxy-2), (rxy-2-3.5), by = -0.5))
         
      }
      if (inputType == "HFD8-100"){ # sensorLength = 9.7
         df = data.frame(position = c(1:8),
                         depth = seq((rxy-2), 
                                     (rxy-2-7), by = -1))
      }
      if (inputType == "Manual"){ # sensorLength = unknown
         df = data.frame(position = c(1:8),
                         depth = seq((rxy-2), 
                                     (rxy-2-(7*sensor_distance)), by = -sensor_distance))
      }
      
      df = df[df$position %in% positions, ]
      
   }
   
   return(df)
   
}

#' Get area and circumference of circular ring to dataframe with positions and depths
#'
add.Props2DepthsHelper = function(depths, swd){

   #depths = depths %>% arrange(desc(depth))
   
   depths = depths %>% 
      mutate(Aring = pi*(depth^2 - lead(depth)^2),
             R = (depth + lead(depth)) / 2,
             Cring = 2*pi * R)
   depths[nrow(depths), "Aring"] = pi*(depths[nrow(depths), "depth"]^2 - swd^2)
   depths[nrow(depths), "R"] =(depths[nrow(depths), "depth"] + swd) / 2
   
   depths[nrow(depths), "Cring"] = 2*pi*depths[nrow(depths), "R"] 
   return(depths)
}


add.Props2Depths = function(depths, swd){
   if (all(depths$depth > 0)){
      depths = add.Props2DepthsHelper(depths = depths, swd = swd)
   } else {
      depths = bind_rows(
         depths[depths[, "depth"] > 0,] %>% add.Props2DepthsHelper(., 0),
         depths[depths[, "depth"] == 0,] %>% 
            mutate(Aring = 0,
                   R = 0,
                   Cring = 0),
         
         depths[depths[, "depth"] < 0,] %>% 
            mutate(depth_helper = depth,
                  depth = abs(depth)) %>% 
            arrange(desc(depth)) %>% 
            add.Props2DepthsHelper(., 0) %>% 
            mutate(depth = depth_helper) %>% 
            select(-depth_helper) %>% 
            arrange(position)
      )
   }
   return(depths)
}

########### CLEAN #############

remove.outlier <- function(data, data.vector){
   d = data[, data.vector]
   Q <- quantile(d, probs=c(.25, .75), na.rm = T)
   iqr <- IQR(d)
   up <-  Q[2] + 1.5 * iqr # Upper Range  
   low <- Q[1] - 1.5 * iqr # Lower Range

   data <- subset(data, data[, data.vector] > low & 
                     data[, data.vector] < up)
   return(data)
}


########### SAVE #############

save.figure = function(name, plotObject, prjName = "PrjName", format = "svg"){
   plotObject = plotObject +
      ggtitle(prjName) +
      theme_bw(base_size = 14)
   
   if (file.exists(paste(name, format, sep = "."))){
      name = paste(name, as.numeric(Sys.time()), sep = "_")
   }
   
   if (format == "svg"){
      svg(filename = paste(name, format, sep = "."),
          width = 12, height = 6)
      print(plotObject)
      dev.off()
      res = ifelse(file.exists(paste(name, format, sep = ".")) &
                      file.info(paste(name, format, sep = "."))$size > 1, T, F)
   }
   
   if (format == "pdf"){
      res = try(ggsave(plotObject, filename = paste(name, format, sep = "."),
                       width = 12, height = 6))
   }
   
   if (format == "jpg"){
      res = try(ggsave(plotObject, filename = paste(name, format,
                                                    sep = "."),
                       width = 12, height = 6, dpi = 900))
   }

   print(res)
   if (is.null(res) || res){
      showNotification("File saved successfully!",
                       type = "message")
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }

}

save.csv = function(name, csvObject){
   res = try(write.csv(csvObject, file = paste(name, ".csv", sep = ""),
                       row.names = FALSE))
   if (is.null(res)){
      showNotification("File saved successfully!",
                       type = "message")
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }
}

