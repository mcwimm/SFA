shinyServer(function(input, output, session) {
  
    ########################
    ### PROJECT SETTINGS ###
    ########################
    
    #### Variables ####
  
    #' Shiny function that returns 'available volumes on the system'
    volumes = getVolumes()

    #' Shiny function to 'to navigate the filesystem'
    folderInput1 <- shinyDirChoose(input, 'folder',
                                   roots=c(wd='.'), 
                                   filetypes=c('', 'txt'))
    
    #' Reactive variable holding the current project path
    projectPath <- reactive({
      parseDirPath(c(wd=getwd()), input$folder)
    })

    #' Reactive variable holding the current project name
    projectName <- reactive({
      return(tail(unlist(strsplit(as.character(projectPath()), "/")), 1))
    })
    
    #' Reactive variable holding the name of plot titles 
    #' (for saved plots)
    #' If not defined in the UI it returns "", i.e. no title appears
    figTitle <- reactive({
        return(input$figTitle)
    })
    
    #' Reactive variable holding the name appended to files
    #' If not defined in the UI it returns "", i.e. nothing 
    #' is appended
    fileAppendix <- reactive({
      return(input$fileAppend) 
    })
    
    #' Reactive variable holding ggplot theme
    #' Can be defined in UI
    plot_theme <- reactive({
      themes[[input$figTheme]]
      })

    #' Reactive variable holding fill colors to be used in 
    #' all plots with discrete data
    #' Can be defined in UI
    fillcolors_react = reactive({
      return(get.fillcolors(ui.input = input))
    })

    #' Global function (accessible from other scripts) 
    #' that returns a set of N fillcolors
    fillcolors <<- function(N){
      col = fillcolors_react()
      return(col[1:N])
    }
    

    #### UI output ####
    
    #' Function to render all ggplots with defined theme
    output$theme_output <- renderUI({ 
      req(input$figTheme)
      theme_set(plot_theme())
      NULL
    })
    
    #' Show project path in Project Settings > Project
    #' if a project folder is selected
    output$prjDir <- renderPrint({
        print(projectPath())
    })
    
    
    #### Buttons ####
    
    #' Button to create a project (Project Settings > Project)
    #' Requires a folder to be selected (Folder select)
    #' If directory does not exist create two folders:
    #' 'csv-files', 'graphics'
    #' Sets project name = project folder name
    observeEvent(input$crtPrj, {
      # If no folder have been selected show error
      if (!isTruthy(input$folder)){
          showNotification("Please choose a directory first!",
                           type = "error")
      } else{
          req(input$folder)
          csvPath = paste(projectPath(),
                          "/csv-files/", sep = "")
          figPath = paste(projectPath(),
                          "/graphics/", sep = "")
          if (!dir.exists(csvPath)){
              dir.create(csvPath)
          }
          if (!dir.exists(figPath)){
              dir.create(figPath)
          }
          showNotification("Project set successfully!",
                           type = "message")
      }
      output$prjName <- renderPrint({
          print(projectName())
      })
    })
    
    
    ############
    ### DATA ###
    ############
    
    #### Variables ####

    #' Reactive variable holding raw sap flow data
    #' If no data set is defined, use default data set
    rawData <- reactive({
      if (is.null(input$file1)){
        defaultData = "./tests/ICT_rawdata.csv"
        print("Default data")
        return(get.temperatures.ICT(defaultData,
                                    sep = ",",
                                    skip = 10))
      } else {
        return(get.rawData(input))
      }
    })
    
    #' Reactive variable holding sap flow data in long format
    #' Unfiltered
    #' Transformation of data set from wide to long depends on
    #' input type: raw temperature (HFD sensor data), 
    #' temperature differences
    deltaTempLongNoFilter <- reactive({
      if (input$inputType == "HFD_raw"){
        d = get.delta.from.temp(rawData(), positions())
      }
      if (input$inputType == "HFD_delta"){
        d = get.delta.temp(rawData(), positions())
      }
      return(d)
    })
  
    #' Create empty reactive value with a placeholder for the
    #' data set in long format
    values <- reactiveValues(deltaTempLong = NULL)
    
    #' Reactive variable holding long-format data
    #' Assigned to reactive value if empty
    deltaTempLong <- reactive({
      if (is.null(values$deltaTempLong)){
        values$deltaTempLong <- deltaTempLongNoFilter()
      }
      return(values$deltaTempLong)
    })
  
    #' Reactive variable holding long-format data for
    #' specific UI-selected sensor position
    deltaTempLong.depth <- reactive({
      deltaTempLong() %>% 
        filter(position == input$kPositionSelect)
    })
    
    #' Reactive variable holding sensor positions
    #' (a vector numbering the sensors) derived from input file
    positions <- reactive({
      # If an input file is given wait until 'set data' button
      # is pressed
      if (!is.null(input$file1)){  
        req(input$setData)
      } 
      h = update.positions(data = rawData(), 
                           ui.input = input, 
                           reactive.value = values)
      values = h[[1]]
      positions = h[[2]]
      return(positions)
    })
    
    depths <- reactive({
      if (!is.null(input$file1)){
        req(input$setData)
      }
      return(update.depths(ui.input = input,
                           positions = positions(),
                           sensor_distance = sensor.dist(),
                           swd = sapWoodDepth()))
    })
    

    #### FILTER ####
    
    filterData <- function(d){

      d$doy <- as.numeric(d$doy)
      
      # filter by day/ doy and day time
      minDoy = as.numeric(strftime(input$daterange[1], format = "%j"))
      maxDoy = as.numeric(strftime(input$daterange[2], format = "%j"))
      
      start = input$timerangeStart
      end = input$timerangeEnd
      
      d = d %>%
        filter(doy >= minDoy) %>%
        filter(doy <= maxDoy)
      
      d = d %>%
        filter(dTime >= start) %>% 
        filter(dTime <= end)
      
      # remove outlier
      if (input$removeOutlier){
        d = remove.outlier(d, input$filterPlot_X)
      }
      
      # remove na-values
      if (input$removeNA){
        d = d[complete.cases(d), ]
      }
      
      # filter temperature filters by range
      dTSymMin = ifelse(is.na(input$dTSymMin), -Inf, input$dTSymMin)
      dTSymMax = ifelse(is.na(input$dTSymMax), Inf, input$dTSymMax)
      dTasMin = ifelse(is.na(input$dTasMin), -Inf, input$dTasMin)
      dTasMax = ifelse(is.na(input$dTasMax), Inf, input$dTasMax)
      dTsym.dTasMin = ifelse(is.na(input$dTsym.dTasMin), -Inf, input$dTsym.dTasMin)
      dTsym.dTasMax = ifelse(is.na(input$dTsym.dTasMax), Inf, input$dTsym.dTasMax)

      d = d %>%
        filter(dTSym >= dTSymMin) %>%
        filter(dTSym <= dTSymMax)

      d = d %>%
        filter(dTas >= dTasMin) %>%
        filter(dTas <= dTasMax)

      d = d %>%
        filter(dTsym.dTas >= dTsym.dTasMin) %>%
        filter(dTsym.dTas <= dTsym.dTasMax)

      # filter by sensor positions
      if (input$sensorFilter != ""){
        sensorFilter = as.numeric(unlist(strsplit(input$sensorFilter,",")))
        d = d %>%
          filter(position %in% sensorFilter)
      }
      

      # print remaining size of data set
      # print(paste("Remaining size of data set: ", nrow(d)))
      print(nrow(d))
      return(d)
    }

    observeEvent(input$LoadFilter, {
      values$deltaTempLong <- deltaTempLongNoFilter()
    })
    
    observeEvent(input$FilterApply, {
      print("Within filter apply")
      print(paste("nrow(data) before  ", nrow(data)))
      
      temp <- filterData(values$deltaTempLong)
      
      print(paste("nrow(data) after  ", nrow(temp)))
      values$deltaTempLong <- temp
      
    })
    
    observeEvent(input$FilterDelete, {
      print("Within filter delete")
      
      temp <- deltaTempLongNoFilter()
      
      values$deltaTempLong <- temp
      
      print(paste("nrow(data) after filter delete ", nrow(temp)))
      
      
    })
    
    #### UI ####

    minMaxDatetime <- reactive({
      if (!is.null(input$file1)){
        req(input$setData)
        print("Require set data1")
      } 
      # d = deltaTempLong()
      d = rawData()
      minDate = as.Date(d[which.min(as.POSIXct(d$datetime)),
                          "datetime"])
      maxDate = as.Date(d[which.max(as.POSIXct(d$datetime)),
                          "datetime"])
      print(c(minDate, maxDate))
      return(c(minDate, maxDate))
    })
    
    observeEvent(input$LoadFilter, {
      if (!is.null(input$file1)){
        req(input$setData)
      } 
      
      output$filterOptions <- renderUI({
        req(input$LoadFilter)
        
        tagList(
          # Date and time range
          h4(strong("Time filters")),
          
          fluidRow(# Date
            column(2,
                   p(strong('Date'))),
            column(10,
                   dateRangeInput("daterange", "Range")
            )),
          
          fluidRow(# Time of day
            column(2,
                   p(strong('Time'))),
            column(5, 
                   numericInput("timerangeStart", "Start", value = 0)),
            column(5, numericInput("timerangeEnd", "End", value = 24)),
          ),
          
          br(), 
          
          # Temperature ranges
          h4(strong("Temperature ranges")),
          fluidRow(# dTSym
            column(4,
                   p(strong('dTSym'))),
            column(4, 
                   numericInput("dTSymMin", "Min", value = Inf)),
            column(4, numericInput("dTSymMax", "Max", value = Inf)),
          ),
          
          fluidRow(# dTas
            column(4,
                   p(strong('dTas'))),
            column(4, 
                   numericInput("dTasMin", "Min", value = Inf)),
            column(4, numericInput("dTasMax", "Max", value = Inf)),
          ),
          
          fluidRow(# dTsym.dTas
            column(4,
                   p(strong('dTsym.dTas'))),
            column(4, 
                   numericInput("dTsym.dTasMin", "Min", value = Inf)),
            column(4, numericInput("dTsym.dTasMax", "Max", value = Inf)),
          ),
          
          # Sensor filters
          h4(strong("Sensor positions")),
          fluidRow(# sensor positions
            column(12,
                   textInput("sensorFilter", "",
                 placeholder = "Sensor positions as vector (comma delimited): 1, 2, 3"))
            ),
          
          
          br(), 
          
          # General filters
          checkboxInput("removeOutlier", "Remove outliers of plot variable", F),
  
          checkboxInput("removeNA", "Remove NA-rows", F),
          
          # Buttons
          fluidRow(
            column(5, (actButton("FilterApply", "Apply filter", "update"))),
            column(5, (actButton("FilterDelete", "Delete filter", "update"))),
          ),
          
          textOutput("dataPoints")
        )
      })
      
      updateDateRangeInput(session, "daterange",
                           start = minMaxDatetime()[1],
                           end = minMaxDatetime()[2],
                           min = minMaxDatetime()[1],
                           max = minMaxDatetime()[2])

    })
    
    
    

    
    #### Table outputs #####

    output$raw.wide <- DT::renderDataTable({ # raw data
      #print(str(rawData()))
        return(rawData())
    }, options = list(scrollX = TRUE))
    
    output$raw.long <- DT::renderDataTable({ # raw data
      return(deltaTempLong() %>% 
               mutate_if(is.numeric, round, 3))
    }, options = list(scrollX = TRUE))
    
    output$depth.table <- DT::renderDataTable({ # sensor positions etc
      d = depths() %>%
        mutate_at(vars(3, 4, 5), round, 1) %>%
        select(-R) %>%
        `colnames<-` (c("Position", "Sensor R (cm)", "Area (cm²)",
                        "Circ. (cm)"))
      return(d)
    }, options = list(scrollX = TRUE))
    
    
    #### Text output ####
    
    output$positions <- renderPrint({
        # cat("As atomic vector:\n")
        print(positions())
    })
    
    
    output$depths <- renderPrint({
      # cat("As atomic vector:\n")
      d = depths() %>% mutate_at(vars(3, 4, 5), round, 1) %>% 
        select(-R) %>%
        `colnames<-` (c("Position", "Sensor R (cm)", "Area (cm²)",
                        "Circ. (cm)"))
      print(d)
    })
    
    output$dataPoints <- renderText({
      n_diff = nrow(deltaTempLongNoFilter()) - nrow(deltaTempLong())
      # paste(nrow(deltaTempLong()), " data points remaining.")
      paste(n_diff, " data points removed.")
    })
    
    #### Graphics ####

    filterPlot <- reactive({
      plot.histogram(data = deltaTempLong(), 
                     ui.input = input)
    })
    
    
    deltaTfacetWrap <- reactive({
        plot.deltaTfacetWrap(data = deltaTempLong(), 
                             ui.input = input)
    })
    
    deltaTSingle <- reactive({
        plot.singleTemperature(data = deltaTempLong(),
                               ui.input = input)
    })
    
    output$filterPlot <- renderPlot({
      filterPlot()
    })
    
    output$deltaTfacetWrap <- renderPlot({
      deltaTfacetWrap()
    })
    
    output$deltaTSingle <- renderPlot({
      deltaTSingle()
    })
    
    
    #### Buttons ####
    
    observeEvent(input$save_dat_upl, {
        csvObject = deltaTempLong()
        path = paste(projectPath(), 
                     "/csv-files/",
                     "deltaT_longFormat", sep = "")
        save.csv(path, csvObject, fileAppendix())
        
    })
    
    observeEvent(input$save.deltaTfacetWrap, {
        name = paste(projectPath(),
                     "/graphics/",
                     "deltaT_", input$rawPlot.facet, sep = "")
        obj = deltaTfacetWrap()
        save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    observeEvent(input$save.deltaTSingle, {
        v = paste(input$rawPlot.xcol, 
                  input$rawPlot.ycol, 
                  input$rawPlot.col,  
                  input$rawPlot.shape,
                  input$rawPlot.facet, sep = "-")
        
        name = paste(projectPath(),
                     "/graphics/",
                     "tempDiff_", v, sep = "")
        obj = deltaTSingle()
        save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })

    # button to save filtered data, long format
    observeEvent(input$save_dat_filter, {
      csvObject = values$deltaTempLong
      path = paste(projectPath(), 
                   "/csv-files/",
                   "deltaT_longFormat_filtered", sep = "")
      save.csv(path, csvObject, fileAppendix())
    })
    
    
    # button to save figure shown in filter window
    observeEvent(input$save_dat_filter_fig, {
      name = paste(projectPath(),
                   "/graphics/",
                   as.character(input$filterPlot_type), "_",
                   "filtered_",
                   as.character(input$filterPlot_X), "_",
                   as.character(input$filterPlot_col), sep = "")

      obj = filterPlot()
      save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    
    ####################
    ### K-ESTIMATION ###
    ####################
    
    #### UI #####
    
    output$kPositionSelect <- renderUI({
      # load current data set
      d = values$deltaTempLong
      pos = positions()
      sel = pos[1]
      radioButtons("kPositionSelect", "Sensor position",
                   choices = pos,
                   selected = sel, inline = T)
    })
    
    
    output$xRangeSlider <- renderUI({

      min = round(min(deltaTempLong.depth()$dTsym.dTas), 2)
      max = round(max(deltaTempLong.depth()$dTsym.dTas), 2)
      
      tagList(
        conditionalPanel(
          condition = "input.k1Plot_scales == 'TRUE'",
          fluidRow(
            column(6, numericInput("k1Plot.x.min", "Min x-value", min)),
            column(6, numericInput("k1Plot.x.max", "Max x-value", max))
          )
        ))
    })
    
    
    #### Variables ####
    
    cleanedDataAndKvalues <- reactive({ # get cleaned data for regression plot
        d = deltaTempLong() %>%
            filter(position == input$kPositionSelect)
        
        if (input$dTimeFilter){
          if (nightTimeStart < nightTimeEnd){
            d = d %>% 
              filter(dTime >= input$kRegressionTime.start & dTime <= input$kRegressionTime.end) 
          } else {
            d = d %>% 
              filter(dTime >= input$kRegressionTime.start | dTime <= input$kRegressionTime.end) 
          }

        }
        return(clean.data.iteration(d))
    })
    
    
    kValue <- reactive({     # get k value by selected method for selected position
        if (input$kMethod == "manual"){
            kManual = input$kManual
        }
        if (input$kMethod == "csv"){
            d = kFromCsv()
            kManual = d[d$position == input$kPositionSelect, "k"]
        }
        
        get.kByMethod(data = deltaTempLong(),
                      ui.input = input,
                      kManual = kManual)
    })

    kComplete <- reactive({  # get k-values for all positions for closest, regression
        get.kByMethodAll(deltaTempLong(),
                         ui.input = input)
    })
    
    kFromCsv <- reactive({
      req(input$file2)
      kcsv = get.csvKvalues(ui.input = input)
      return(kcsv)
        
    })
    
    #' Check if any k-values have been set
    click <- reactive(({
      click = input$setK[1] + input$setKfromCsv[1] + input$setKfromRegression[1] +
        input$setKfromClosest[1]
    }))
    
    #### Store and display selected k-values ####
    
    # create reactive value to store selected k-values
    values <- reactiveValues(df_data = NULL)  
    
    #' Button to store single selected k-value in data.frame
    observeEvent(input$setK, {  
      click = click()
      if (click == 1 && is.null(input$file2)){
        values$df_data <-  data.frame(position = positions(),  
                                      method = rep(NA),
                                      k = rep(NA))
      }
      
      values$df_data[values$df_data$position == input$kPositionSelect, 2:3] <- cbind(
        method = as.character(input$kMethod),
        k = round(kValue(), 3))
    })
    
    #' Button to use uploaded k-Values
    observeEvent(input$setKfromCsv, {
      values$df_data <-  data.frame(position = positions(),  
                                    method = rep(NA),
                                    k = rep(NA))
      csvK = kFromCsv()

      for (pos in unique(csvK[, "position"])){
        values$df_data[values$df_data$position == pos, 2:3] <- cbind(
          method = ifelse(is.null(as.character(csvK$method)), "csv",
          as.character(csvK[csvK$position == pos, "method"])),
          k = csvK[csvK$position == pos, "k"])
      }
    })
   
    #' Button to use all k-Values from automatic regression
    observeEvent(input$setKfromRegression, {
      values$df_data <-  data.frame(position = positions(),  
                                    method = rep(NA),
                                    k = rep(NA))
      
      regK = kComplete()$regression %>% round(., 3)
      
      method_name = "auto. regression"
      if (input$dTimeFilter){
        method_name = paste(method_name, " (",
                            input$kRegressionTime.start, " - ",
                            input$kRegressionTime.end, ")",
                            sep = "")
      }
      for (pos in unique(regK[, "position"])){
        values$df_data[values$df_data$position == pos, 2:3] <- cbind(
          method = method_name,
          k = regK[regK$position == pos, "k"])
      }
    })
    
    #' Button to use all k-Values from closest estimate
    observeEvent(input$setKfromClosest, {
      values$df_data <-  data.frame(position = positions(),  
                                    method = rep(NA),
                                    k = rep(NA))
      
      closK = kComplete()$closest %>% round(., 3)
      
      for (pos in unique(closK[, "position"])){
        values$df_data[values$df_data$position == pos, 2:3] <- cbind(
          method = "closest",
          k = closK[closK$position == pos, "k"])
      }
    })

    #### Text outputs ####

    # output current k-value (depends on selected position and method)
    output$kCurrent <- renderPrint({ 
        paste("K-value", round(kValue(), 3))
    })
    
    
    #### Table outputs ####
    
    output$kSelected <- DT::renderDataTable({  
      # display selected k-values
        return(values$df_data)
    }, options = list(scrollX = TRUE))
    
    
    output$kRegression <- DT::renderDataTable({  
      # display estimated k-values - by regression
        return(kComplete()$regression  %>% round(., 2))
    }, options = list(scrollX = TRUE))
    
    
    output$kClosest <- DT::renderDataTable({  # output closest k-estimates
        return(kComplete()$closest %>% round(., 2))
    }, options = list(scrollX = TRUE))


    output$uploadedKvalues <- DT::renderDataTable({  # output closest k-estimates
        return(kFromCsv())
    }, options = list(scrollX = TRUE))
    
    
    
    #### Graphics ####
    kNightTime <- reactive({
      plot.nighttime(data.complete = deltaTempLong.depth())

    })
    
    kplot1 <- reactive({
      plot.kEst1(data.complete = deltaTempLong.depth(),
                 data.adj = cleanedDataAndKvalues()[[1]],
                 ui.input = input)
    })
    
    kplot2 <- reactive({
      plot.kEst2(data.complete = deltaTempLong.depth(),
                 data.adj =cleanedDataAndKvalues()[[1]],
                 k = kValue(),
                 ui.input = input)
    })
    
    kplot3 <- reactive({
        plot.kEst3(data.complete = deltaTempLong.depth(), 
                   data.adj = cleanedDataAndKvalues()[[1]],
                   k = kValue(),
                   ui.input = input)
    })
    
    output$kNightTimePlot <- renderPlot({ kNightTime() })
    output$kvaluePlot1 <- renderPlot({ kplot1() })
    output$kvaluePlot2 <- renderPlot({ kplot2() })
    output$kvaluePlot3 <- renderPlot({ kplot3() })
    
    #### Buttons ####
    # save data
    
    observeEvent(input$save.kValues, { # save selected k-values as csv
        path = paste(projectPath(), 
                     "/csv-files", sep = "")
        csvObject = values$df_data
        save.csv(paste(path, "/k-values", sep = ""), csvObject, fileAppendix())
    })
    
    # save figures
    observeEvent(input$save.kPlots, {
        name = paste(projectPath(),
                     "/graphics/",
                     sep = "")
        figTitle = figTitle()
        fileAppendix = fileAppendix()
        
        save.figure(paste(name, "k_fig1_position_", input$kPositionSelect, sep = ""), 
                    kplot1(), figTitle, fileAppendix, input$figFor)
        save.figure(paste(name, "k_fig2_position_", input$kPositionSelect, sep = ""), 
                    kplot2(), figTitle, fileAppendix, input$figFor)
        save.figure(paste(name, "k_fig3_position_", input$kPositionSelect, sep = ""), 
                    kplot3(), figTitle, fileAppendix, input$figFor)
    })

    ####################
    ##### SAP FLOW  ####
    ####################
    #### Variables ####
    
    sapWoodDepth <- reactive({
      if (input$sapWoodDepth == 0){
        if (input$stemCircumference == 0){
          swd = input$stemDiameter/2 - input$barkThickness - input$heartWoodDepth
        } else {
          swd = input$stemCircumference / (2*pi) - input$barkThickness -
            input$heartWoodDepth
        }} else {
        swd = input$sapWoodDepth
      }
      return(swd)
    })
    
    
    sapFlowDens <- reactive({
      #req(input$setK, input$setKfromRegression)
      kValues = values$df_data
      kValues[, "k"] = as.numeric(kValues[, "k"])
      positions = unique(kValues[!is.na(kValues$k), ]$position)
      data = deltaTempLong()
      data = data[data$position %in% positions, ]
      data = merge(data, kValues[, c("position", "k")], by = "position")
      data = data[!is.na(data$datetime), ]
      
      data = get.sapFlowDensity(method = "HFD",
                                data = data,
                                sapWoodDepth = sapWoodDepth(),
                                ui.input = input)
      return(data)
      
    })
    
    sensor.dist <- reactive({
      if (input$sensorType == "HFD8-50"){
        return(0.5)
      }
      if (input$sensorType == "HFD8-100"){
        return(1)
      }
      if (input$sensorType == "Manual"){
        return(input$distInput)
      }
    })
    
    sapFlow <- reactive({
      methods <- list("treeScaleSimple1" = input$treeScaleSimple1,
                      "treeScaleSimple2" = input$treeScaleSimple2,
                      "treeScaleSimple3" = input$treeScaleSimple3)

      data = sapFlowDens()
      data = merge(data, depths(), by = "position")
      
      for (m in c(1:length(methods))){
        if (methods[[m]]){
          method = names(methods)[m]
          data = get.sapFlowByMethod(data = data,
                                     method = method, 
                                     swd = sapWoodDepth()) 

        } 
      }        

      print(paste("SAP WOOD DEPTH  ", sapWoodDepth()))
      print(paste("SAP WOOD AREA", mean(data$SWDarea)))
      
      return(data)
    })
    
    #' Calculate daily tree water use in kg per h and kg per day
    treeWaterUse <- reactive({
      if (click() > 0){
        get.treeWaterUseByMethod(data = sapFlow(),
                                 input = input)
      } else {
        return(data.frame(x = "No k-values have been set yet."))
      }
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sfIndex, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexComplete", sep = "")
        obj = sapFlowIndex()
        save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    observeEvent(input$save.sfIndex.day, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexDaily", sep = "")
        obj = sapFlowIndex.Day()
        save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    
    #### Graphics ####
    sapFlowIndex <- reactive({
      plot.sapFlowIndex(data = deltaTempLong(), 
                        ui.input = input)
    })
    
    sapFlowIndex.Day <- reactive({
      plot.sapFlowIndex.Day(data = deltaTempLong(), 
                            ui.input = input)
    })
    
    sapFlowDensityPlot = reactive({
      print(paste('click ', click()))
      if (click() == 0 && is.null(input$file2)){
        ggplot() +
          annotate(geom="text", x=5, y=5, 
                   label="No k-values have been set yet.",
                   color="red", size = 8) +
          theme_void()
      } else {
        d = sapFlowDens()
        
        # check if sap flow density is Inf
        # helper variable; if 0 no sap flow density data is avail.
        SFDensity = nrow(d)
        if (input$sapFlowDensityPlot.y == "SFDsw"){
          SFDensity = nrow(d %>% mutate(all = n()) %>% 
            filter(abs(SFDsw) != Inf))
          
        }

        # show error message if sap flow density haven't been calculated (i.e. is Inf)
        if (SFDensity == 0){
          ggplot() +
            annotate(geom="text", x=5, y=5, 
                     label="Wood properties are missing.",
                     color="red", size = 8) +
            theme_void()
        } else{
          plot.sapFlowDensity(data = d, 
                              ui.input = input)
        }
      }
      
    })
    
    sapFlowDensityPlot.Boxplot = reactive({
      print(paste('click ', click()))
      if (click() == 0 && is.null(input$file2)){
        ggplot() +
          annotate(geom="text", x=5, y=5, 
                   label="No k-values have been set yet.",
                   color="red", size = 8) +
          theme_void()
      } else {
        d = sapFlowDens()
        
        # check if sap flow density is Inf
        # helper variable; if 0 no sap flow density data is avail.
        SFDensity = nrow(d)
        if (input$sapFlowDensityPlot.y == "SFDsw"){
          SFDensity = nrow(d %>% mutate(all = n()) %>% 
                             filter(abs(SFDsw) != Inf))
          
        }
        
        # show error message if sap flow density haven't been 
        # calculated (i.e. is Inf)
        if (SFDensity == 0){
          ggplot() +
            annotate(geom="text", x=5, y=5, 
                     label="Wood properties are missing.",
                     color="red", size = 8) +
            theme_void()
        } else{
          names(d)
          d %>% 
            ggplot(., aes(x = factor(position), y = SFS, 
                          col = factor(position)))+
            geom_boxplot() +
            # scale_y_log10() +
            facet_wrap(~ doy)
          
        }
      }
      
    })
    
    
    sapFlowTreePlot <- reactive({
      print(paste('click ', click()))
      if (click() == 0 && is.null(input$file2)){
        ggplot() +
          annotate(geom="text", x=5, y=5, 
                   label="No k-values have been set yet.",
                   color="red", size = 8) +
          theme_void()
      } else {
        if (sapWoodDepth() == 0){
          ggplot() +
            annotate(geom="text", x=5, y=5, 
                     label="Wood properties are missing (see 'Project settings').",
                     color="red", size = 8) +
            theme_void()
        } else {
          plot.sapFLowRate(data = sapFlow(), ui.input = input)
        }
      
      }
    })
    
    
    output$sapFlowIndex <- renderPlot({
      sapFlowIndex()
    })
    
    output$sapFlowIndex.Day <- renderPlot({
      sapFlowIndex.Day()
    })
    
    output$sapFlowDensity <- renderPlot({
      sapFlowDensityPlot()
    })
    
    output$sapFlowDensity.Boxplot <- renderPlot({
      sapFlowDensityPlot.Boxplot()
    })
    
    output$SapFlowPlot <- renderPlot({
      sapFlowTreePlot()
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sapFlowDensityPlot, {
      name = paste(projectPath(),
                   "/graphics/",
                   "sf_", input$sapFlowDensityPlot.y, sep = "")
      obj = sapFlowDensityPlot()
      save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    observeEvent(input$save.sapFlowDensity, {
      name = paste(projectPath(),
                   "/csv-files/",
                   "sapFlowDensity", sep = "")
      obj = sapFlowDens()
      save.csv(name, obj, fileAppendix())
    })
    
    observeEvent(input$save.SapFlow, {
      name = paste(projectPath(),
                   "/graphics/",
                   "sapFlow_scaled", sep = "")
      obj = sapFlowTreePlot()
      save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    observeEvent(input$save.SapFlowCsv, {
      name = paste(projectPath(),
                   "/csv-files/",
                   "sapFlow_scaled", sep = "")
      obj = sapFlow()
      save.csv(name, obj, fileAppendix())
    })
    
    
    observeEvent(input$save.TreeWaterUseCsv, {
      name = paste(projectPath(),
                   "/csv-files/",
                   "treeWaterUsePerDay", sep = "")
      obj = treeWaterUse()
      save.csv(name, obj, fileAppendix())
    })
    
    
    #### Table ####
    
    # Table with daily tree water use
    output$twu.table <- DT::renderDataTable({ 
      treeWaterUse()
    }, options = list(scrollX = TRUE))
})
