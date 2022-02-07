#' Wrapper function to get uploaded raw data based on selected
#' method
#' @param UI-input: file, file-args, method, etc.
#' @return data.frame
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
#' @param file: uploaded file
#' @param sep: symbol to use as separator
#' @param skip: number of rows to skip
#' @return data.frame
get.temperatures.ICT = function(file, sep, skip){
   rawData <- read.csv(file,
                       header = TRUE, sep = sep, 
                       fileEncoding="latin1",
                       skip = skip)

   if (length(colnames(rawData)) > 1){
      col = grep("Exter", colnames(rawData) )[1]
      rawData = rawData[, c(1:col)]
      
      rawData = unify.datetime(rawData)
   }
   
   return(rawData)
}

unify.datetime = function(rawData){
   if ("datetime" %in% tolower(names(rawData))){
      print("Transform datetime to date and time")
      datetimeformat = get.datetime.format(rawData[1,]$datetime)
      rawData$datetime = as.POSIXct(rawData$datetime,
                                    format = datetimeformat)
      rawData$date = strftime(rawData$datetime, format="%d.%m.%Y")
      rawData$time = strftime(rawData$datetime, format="%H:%M:%S")
   } else {
      if (all(c(tolower("time"), tolower("date")) %in% tolower(names(rawData)))){
         print("Add datetime based on date and time")
         
         date_col = rawData[grepl("(?i)date", colnames(rawData))]
         time_col = rawData[grepl("(?i)time", colnames(rawData))]
         # Remove upper case columns if exist
         rawData[, names(date_col)] = NULL
         rawData[, names(time_col)] = NULL
         # Define columns with lower case names for further processing
         rawData$date = date_col[[1]]
         rawData$time = time_col[[1]]

         datetimeformat = get.datetime.format(rawData[1,]$date, rawData[1,]$time)
         rawData = rawData %>% 
            rowwise() %>% 
            mutate(datetime = as.POSIXct(x = paste(date, time),
                                         format = datetimeformat))
      }
   }
   
   # add hour of day and date of year
   rawData$dTime = convertTimeToDeci(as.character(rawData$time))
   rawData$doy <- as.numeric(strftime(rawData$datetime, format = "%j"))
   
   # Reorder columns
   rawData = data.frame(rawData) %>% 
      relocate(datetime, date, time, doy, dTime)
   return(rawData)
}

#' Reads temperature differences from processed ICT-data file.
#' @param file: uploaded file
#' @param sep: symbol to use as separator
#' @param skip: number of rows to skip
#' @return data.frame
get.temp.differences.ICT = function(file, sep, skip){
   rawData <- read.csv(file,
                       header = TRUE, sep = sep, 
                       fileEncoding="latin1",
                       skip = skip)
   rawData = unify.datetime(rawData)

   return(rawData)
}

#' Function to convert string to data-time format
#' @param date: datetime or date as string
#' @param time: time as string
#' @return datetime object
get.datetime.format = function(date, time=""){
   # datetime = rawData[1, c("Date", "Time")]
   if (!is.na(as.POSIXct(x = paste(date, time), 
                     format="%d.%m.%Y %H:%M:%S"))){
      return("%d.%m.%Y %H:%M:%S")
   }
   if (!is.na(as.POSIXct(x = paste(date, time), 
                         format="%d/%m/%Y %H:%M:%S"))){
      return("%d/%m/%Y %H:%M:%S")
   }
   if (!is.na(as.POSIXct(x = paste(date, time), 
                         format="%d.%m.%Y %H:%M"))){
      return("%d.%m.%Y %H:%M")
   }
   if (!is.na(as.POSIXct(x = paste(date, time), 
                         format="%d/%m/%Y %H:%M"))){
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
#' @param rawData: data.frame containing raw data in wide-format
#' @param position: numeric indicating selected position
#' @return data.frame
get.delta.from.temp.depth <- function(rawData, position){
   raw_temperatures <- rawData[, grepl("Temp", colnames(rawData))]
   raw_temperatures <- raw_temperatures[, !grepl("Batt", colnames(raw_temperatures))]
   
   reg.U <- paste(position, "\\S*(?i)U", sep=".")
   reg.L <- paste(position, "\\S*(?i)L", sep=".")
   reg.S <- paste(position, "\\S*(?i)S", sep=".")
   
   t_up <- matrix(as.numeric(raw_temperatures[, grepl(reg.U,
                                               colnames(raw_temperatures))]))
   
   t_low <- matrix(as.numeric(raw_temperatures[, grepl(reg.L,
                                                colnames(raw_temperatures))]))
   
   t_side <- matrix(as.numeric(raw_temperatures[, grepl(reg.S,
                                                 colnames(raw_temperatures))]))
   

   if ((ncol(t_up) != 1) | (ncol(t_low) != 1) | (ncol(t_side) != 1)){
      print(paste("There is more than one temperature dataset for position ", position,
            ". Please check your raw data file.", sep = ""))
   }

   delta.temp <- data.frame(
                     datetime = rawData[, "datetime"],
                     time = rawData[, "time"],
                     doy = rawData[, "doy"],
                     dTime = rawData[, "dTime"],
                     dTas = t_side[, 1] - t_low[, 1],
                     dTsa = t_up[, 1] - t_side[, 1],
                     dTSym = t_up[, 1] - t_low[, 1]
                  )
   colnames(delta.temp) = c("datetime", "time", "doy", "dTime",
                            "dTas", "dTsa", "dTSym")
   
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
#' @param rawData: data.frame containing raw data in wide-format
#' @param positions: vector containing all positions
#' @return data.frame
get.delta.from.temp = function(rawData, positions){
   delta.temp = do.call(rbind, 
                        lapply(positions, function(x)
      cbind(position = x, 
            get.delta.from.temp.depth(rawData = rawData, position = x))))

   return(delta.temp)
}


#' Function to get temperature differences
#' @description  Helper function for get.delta.temp
#' @param rawData: data.frame containing raw data in wide-format
#' @param position: numeric for selected position
#' @return data.frame
get.delta.temp.depth = function(rawData, position){
   # grep everything containg string and position, not case sensitive,
   # ignore sep. between string & position
   reg.sym <- paste("(?i)dTSym\\S*", position, sep = "")
   reg.asym <- paste("(?i)dTas\\S*", position, sep = "")

   sym <- matrix(rawData[, grepl(reg.sym, colnames(rawData))])
   asym <- matrix(rawData[, grepl(reg.asym, colnames(rawData))])
   
   if ((ncol(sym) != 1) | (ncol(asym) != 1)){
      print(paste("There is more than one temperature dataset for position ", position,
                  ". Please check your raw data file.", sep = ""))
   }
   delta.temp <- data.frame(
      datetime = rawData[, "datetime"],
      time = rawData[, "time"],
      doy = rawData[, "doy"],
      dTime = rawData[, "dTime"],
      dTas = asym,
      dTSym = sym
   )
   colnames(delta.temp) = c("datetime", "time", "doy", "dTime",
                            "dTas", "dTSym")
   
   delta.temp = cbind(delta.temp, 
                      "dTsa" = delta.temp[, "dTSym"] - 
                         delta.temp[, "dTas"],
                      "dTsym.dTas" = delta.temp[, "dTSym"] / 
                         delta.temp[, "dTas"],
                      "timestep" = c(1:nrow(delta.temp)))
   return(delta.temp)
   
}

#' Make long format delta temperature data from wide format
#' 
#' @param rawData: data.frame containing raw data in wide-format
#' @param positions: vector containing all positions
#' @return data.frame
get.delta.temp = function(rawData, positions){
   delta.temp = do.call(rbind, 
           lapply(positions, function(x)
              cbind(position = x, 
                    get.delta.temp.depth(rawData = rawData, 
                                         position = x))))
   return(delta.temp)
}


#' Converts character time HH:MM:SS to decimal time
#' 
#' @param time: time object or character
#' @return numeric
convertTimeToDeci <- function(time){
   dt = sapply(strsplit(time,":"),
               function(x) {
                  x <- as.numeric(x)
                  x[1]+x[2]/60+x[3]/3600
               })
   return(dt)
}

#' Sensor positions
#' 
#' @description 
#' Sensor positions are either extracted from raw data file or
#' entered manually
#' @param dataSource: data.frame
#' @param input: UI-input
#' @return vector
get.positionsFromRawData = function(dataSource, input){

   if (input$inputType == "HFD_raw"){
      reg = "(?i)Asym"
   }
   
   if (input$inputType == "HFD_delta"){
      reg = "(?i)dTSym"
   }
   
   if (input$positionManual){
      positions = as.numeric(unlist(strsplit(input$positionInput,",")))
   } else {
      positions = c(1:ncol(dataSource[, grepl(reg, 
                                          colnames(dataSource))]))
   }
   
   return(positions)
}


#' Sensor positions relative to stem center
#' @description Position of each sensor as distance to the center of the stem. Can be defined manually. Otherwise the distance between center and outer sap wood Rxy is required.
#' @param depthManual: boolean indicating whether depth is defined manually in UI (default: F)
#' @param inputType: character string indicating sensor type
#' @param positions: vector with sensor positions
#' @param rxy: numeric indicating distance between stem center and outer sap wood
#' @param depthInput: character string indicating manual sensor depths
#' @param sensor_distance: numeric indicating distance between sensors
#' @return data.frame
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
#' @param depths: vector indicating sensor depths
#' @param rxy: numeric indicating distance between stem center and outer sapwood
#' @param swd: numeric indicating sapwooddepth in cm
#' @return data.frame
add.Props2DepthsHelper = function(depths, rxy, swd){

   #depths = depths %>% arrange(desc(depth))
   depths = depths %>% 
      mutate(Aring = pi*(depth^2 - lead(depth)^2),
             R = (depth + lead(depth)) / 2,
             Cring = 2*pi * R)
   
   sensor_distance = abs(depths[1, "R"] - depths[2, "R"])
   last_ring_outer = pi * depths[nrow(depths), "depth"]^2
   last_ring_inner = pi * (depths[nrow(depths), "depth"]-sensor_distance)^2 
   
   depths[nrow(depths), "Aring"] = last_ring_outer - last_ring_inner
   depths[nrow(depths), "R"] = depths[nrow(depths), "depth"] - sensor_distance / 2

   depths[nrow(depths), "Cring"] = 2*pi*depths[nrow(depths), "R"]
   # print(depths)
   
   return(depths)
}

#' Function to create data.frame with sensor information and corresponding 
#' areas and circumferences
#' @param depths: vector indicating sensor depths
#' @param rxy: numeric indicating distance between stem center and outer sapwood
#' @param swd: numeric indicating sapwooddepth in cm
#' @return data.frame
add.Props2Depths = function(depths, rxy, swd){
   if (all(depths$depth > 0)){
      depths = add.Props2DepthsHelper(depths = depths, 
                                      rxy = rxy,
                                      swd = swd)
   } else {
      depths = bind_rows(
         depths[depths[, "depth"] > 0,] %>% add.Props2DepthsHelper(., rxy, 0),
         depths[depths[, "depth"] == 0,] %>% 
            mutate(Aring = 0,
                   R = 0,
                   Cring = 0),
         
         depths[depths[, "depth"] < 0,] %>% 
            mutate(depth_helper = depth,
                  depth = abs(depth)) %>% 
            arrange(desc(depth)) %>% 
            add.Props2DepthsHelper(., rxy, 0) %>% 
            mutate(depth = depth_helper) %>% 
            select(-depth_helper) %>% 
            arrange(position)
      )
   }
   return(depths)
}

#' Update sensor positions
#' @param ui.input: UI-inputs
#' @param data: data.frame: long-format data
#' @param reactive.value: reactive values
#' 
#' @return list with reactive values and vector
update.positions = function(data, ui.input, reactive.value){
   positions = get.positionsFromRawData(dataSource = data,
                                        input = ui.input)
   
   # Update sensor positions if a filter was applied to the data set
   if (ui.input$LoadFilter != 0){
      # Case if FilterApply button was activated the first time: use filtered data
      if (!is.null(ui.input$FilterApply)){
         d = reactive.value$deltaTempLong
         positions = unique(d$position)
      }
      # Case if FilterDelete button was activated the first time: use raw data
      if (!is.null(ui.input$FilterDelete)){
         positions = get.positionsFromRawData(dataSource = data, 
                                              input = ui.input)
      }
      # Case if filters have been applied and deleted button was activated
      # the first time
      if ((!is.null(ui.input$FilterApply)) & (!is.null(ui.input$FilterDelete))){
         if (ui.input$FilterApply > ui.input$FilterDelete){
            d = reactive.value$deltaTempLong
            positions = unique(d$position)
         } else{
            positions = get.positionsFromRawData(dataSource = data, 
                                                 input = ui.input)
         }
      }
   } 
   return(list(reactive.value, positions))
}

#' Update sensor depths
#' @param ui.input: UI-inputs
#' @param positions: vector: sensor positions
#' @param sensor_distance: numeric: distance between sensors, mm
#' @param swd: numeric: sapwood depth
#' @return data.frame
update.depths = function(ui.input, positions, sensor_distance, swd){
   # Calculate the distance Rxy from the stem center to the inner bark
   # Prioritize information on sap wood depth over dbh
   if (ui.input$sapWoodDepth != 0){
      rxy = ui.input$sapWoodDepth + ui.input$heartWoodDepth
   } else {
      rxy = ui.input$stemDiameter / 2 - ui.input$barkThickness
   }
   depths = get.depths(depthManual = ui.input$depthManual,
                       inputType = ui.input$sensorType,
                       positions = positions,
                       rxy = rxy,
                       depth = ui.input$depthInput,
                       sensor_distance = sensor_distance)
   # Àdd area and circumference of circular ring
   depths = add.Props2Depths(depths = depths, 
                             rxy = rxy,
                             swd = swd)
   print(paste('Estimated sap wood depth in cm: ', swd, ', rxy: ', rxy))
   return(depths)
}

########### CLEAN #############

#' Remove outlier
#' @description Function that removes outlier of a defined variable
#' @param data: data.frame with long-format data
#' @param data.vector: character indicating column name of selected variable
#' @return data.frame
remove.outlier <- function(data, data.vector){
   d = data[, data.vector]
   Q <- quantile(d, probs=c(.25, .75), na.rm = T)
   iqr <- IQR(d, na.rm = T)
   up <-  Q[2] + 1.5 * iqr # Upper Range  
   low <- Q[1] - 1.5 * iqr # Lower Range

   data <- subset(data, data[, data.vector] > low & 
                     data[, data.vector] < up)
   return(data)
}

#' Filter
#' @description Function to filter uploaded data set
#' @param data: data.frame with long-format data
#' @param ui.input: UI-input
#' @return data.frame
get.filteredData <- function(data, ui.input){
   data$doy <- as.numeric(data$doy)
   # filter by day/ doy and day time
   minDoy = as.numeric(strftime(ui.input$daterange[1], format = "%j"))
   maxDoy = as.numeric(strftime(ui.input$daterange[2], format = "%j"))

   data = data %>%
      filter(doy >= minDoy) %>%
      filter(doy <= maxDoy)

   start = ui.input$timerangeStart
   end = ui.input$timerangeEnd
   if (start < end){
      data = data %>% 
         filter(dTime >= start & dTime <= end) 

   } else {
      data = data %>% 
         filter(dTime >= start | dTime <= end) 

   }

   # remove outlier
   if (ui.input$removeOutlier){
      data = remove.outlier(data, ui.input$filterPlot_X)
   }

   # remove na-values
   if (ui.input$removeNA){
      data = data[complete.cases(data), ]
   }

   # filter temperature filters by range
   dTSymMin = ifelse(is.na(ui.input$dTSymMin), -Inf, ui.input$dTSymMin)
   dTSymMax = ifelse(is.na(ui.input$dTSymMax), Inf, ui.input$dTSymMax)
   dTasMin = ifelse(is.na(ui.input$dTasMin), -Inf, ui.input$dTasMin)
   dTasMax = ifelse(is.na(ui.input$dTasMax), Inf, ui.input$dTasMax)
   dTsym.dTasMin = ifelse(is.na(ui.input$dTsym.dTasMin), -Inf, ui.input$dTsym.dTasMin)
   dTsym.dTasMax = ifelse(is.na(ui.input$dTsym.dTasMax), Inf, ui.input$dTsym.dTasMax)
   
   data = data %>%
      filter(dTSym >= dTSymMin) %>%
      filter(dTSym <= dTSymMax)
   
   data = data %>%
      filter(dTas >= dTasMin) %>%
      filter(dTas <= dTasMax)
   
   data = data %>%
      filter(dTsym.dTas >= dTsym.dTasMin) %>%
      filter(dTsym.dTas <= dTsym.dTasMax)

   # filter by sensor positions
   if (ui.input$sensorFilter != ""){
      sensorFilter = as.numeric(unlist(strsplit(ui.input$sensorFilter,",")))
      data = data %>%
         filter(position %in% sensorFilter)
   }

   return(data)
}

#' Filter UI
#' @description Function to update data filter in UI based on uploaded data set
#' @param ui.input: UI-input
#' @param ui.output: UI-output
#' @return UI-output
update.filter.ui = function(ui.output, ui.input){
   
   ui.output$filterOptions <- renderUI({
      req(ui.input$LoadFilter)
      
      tagList(
         # Date and time range
         h4(strong("Time filters")),
         
         fluidRow(# Date
            column(2, p(strong('Date'))),
            column(10, dateRangeInput("daterange", "Range")
            )),
         
         fluidRow(# Time of day
            column(2, p(strong('Time'))),
            column(5, numericInput("timerangeStart", "Start", value = 0)),
            column(5, numericInput("timerangeEnd", "End", value = 24)),
         ),
         
         # Temperature ranges
         h4(strong("Temperature ranges")),
         fluidRow(# dTSym
            column(4, p(strong('dTSym'))),
            column(4, numericInput("dTSymMin", "Min", value = Inf)),
            column(4, numericInput("dTSymMax", "Max", value = Inf)),
         ),
         
         fluidRow(# dTas
            column(4, p(strong('dTas'))),
            column(4, numericInput("dTasMin", "Min", value = Inf)),
            column(4, numericInput("dTasMax", "Max", value = Inf)),
         ),
         
         fluidRow(# dTsym.dTas
            column(4, p(strong('dTsym.dTas'))),
            column(4, numericInput("dTsym.dTasMin", "Min", value = Inf)),
            column(4, numericInput("dTsym.dTasMax", "Max", value = Inf)),
         ),
         
         # General filters
         checkboxInput("removeOutlier", "Remove outliers of plot variable", F),
         checkboxInput("removeNA", "Remove NA-rows", F),
         
         # Sensor filters
         h4(strong("Sensor positions")),
         fluidRow(# sensor positions
            column(12, textInput("sensorFilter", "",
                                 placeholder = "comma delimited: 1, 2, 3"))
         ),
         
         # Buttons
         fluidRow(
            column(5, (actButton("FilterApply", "Apply filter", "update"))),
            column(5, (actButton("FilterDelete", "Delete filter", "update"))),
         ),
         htmlOutput("dataPoints")
      )
   })
   return(ui.output)
}
########### SAVE #############


#' Save figure
#' @description Handler to save ggplots as as jpg, svg or pdf
#' @return success-message
#' @param name: file name
#' @param plotObject: object to be saved, i.e. ggplot-object
#' @param fileAppendix: character to be appended to file name
#' @param format: file format
#' @param prjName: project name, added as title to plot
save.figure = function(path, name, plotObject, ui.input){
   fileAppendix = ui.input$fileAppend
   format = ui.input$figFor
   
   plotObject = plotObject +
         ggtitle(ui.input$figTitle) +
      theme(text=element_text(size = 14))
   
   # Add appendix to file name and replace whitespace
   if (fileAppendix != ""){
      fileAppendix = gsub(" ", "_", fileAppendix, fixed = TRUE)
      name = paste(name, fileAppendix, sep = "_")
   }
   
   # Check if project directory is defined
   # If not show warning and set path to root directory
   if (!isTruthy(ui.input$folder)){
      file = paste(path, name, sep = "/")
      noti_note = "No project selected. File saved in root directory."
      noti_type = "warning"
      
   } else {
      file = paste(path, "graphics", name, sep = "/")
      noti_note = "File saved successfully!"
      noti_type = "message"
   }

   # Check if file already exists, if yes append unique number
   # based on system time
   if (file.exists(paste(file, format, sep = "."))){
      file = paste(file, as.numeric(Sys.time()), sep = "_")
   }
   
   if (format == "rdata"){
      res = try(save(plotObject, file = paste(file, format, sep = ".")))
   } else {
      res = try(ggsave(plotObject, filename = paste(file, format,
                                                    sep = "."),
                       width = 12, height = 6, dpi = 900))
   }
   
   if (is.null(res) || res){
      showNotification(noti_note, 
                       type = noti_type)
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }
}


#' Save csv
#' @description Handler to save data.frames as csv-file.
#' @return success-message
#' @param name: file name
#' @param csvObject: object to be saved, i.e. data.frame
#' @param fileAppendix: character to be appended to file name
save.csv = function(path, name, csvObject, ui.input){
   fileAppendix = ui.input$fileAppend
   
   if (fileAppendix != ""){
      fileAppendix = gsub(" ", "_", fileAppendix, fixed = TRUE)
      name = paste(name, "_", fileAppendix, ".csv", sep = "")
   } else {
      name = paste(name, ".csv", sep = "")
   }
   
   # Check if project directory is defined
   # If not show warning and set path to root directory
   if (!isTruthy(ui.input$folder)){
      file = paste(path, name, sep = "/")
      noti_note = "No project selected. File saved in root directory."
      noti_type = "warning"
      
   } else {
      file = paste(path, "csv-files", name, sep = "/")
      noti_note = "File saved successfully!"
      noti_type = "message"
   }
   
   res = try(write.csv(csvObject, 
                       file = file,
                       row.names = FALSE))
   if (is.null(res)){
      showNotification(noti_note, 
                       type = noti_type)
   } else {
      showNotification("Error: File not saved!",
                       type = "error")
   }
}
