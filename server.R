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
        data = get.temperatures.ICT(defaultData,
                                    sep = ",",
                                    skip = 10)
      } else {
        data = get.rawData(input)
      }
      return(data)
    })
    
    #' Reactive variable holding sap flow data in long format
    #' Unfiltered
    #' Transformation of data set from wide to long depends on
    #' input type: raw temperature (HFD sensor data), 
    #' temperature differences
    deltaTempLongNoFilter <- reactive({
      data = rawData()
      positions = get.positionsFromRawData(dataSource = data,
                                           input = input)
      if (input$inputType == "HFD_raw"){
        d = get.delta.from.temp(data, positions)
      }
      if (input$inputType == "HFD_delta"){
        d = get.delta.temp(data, positions)
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
    
    #' Reactive variable holding depth of each sensor positions
    #' in cm
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
    
    #' Button to load filter options
    #' Assigns unfiltered, long-format data as reactive 
    #' value 'deltaTempLong'
    observeEvent(input$LoadFilter, {
      values$deltaTempLong <- deltaTempLongNoFilter()
    })
    
    #' Button to apply filter
    #' Assigns filterd, long-format data as reactive 
    #' value 'deltaTempLong'
    observeEvent(input$FilterApply, {
      values$deltaTempLong <- get.filteredData(data = values$deltaTempLong,
                                               ui.input = input)
    })
    
    #' Button to delete filter
    #' Assigns unfiltered, long-format data as reactive 
    #' value 'deltaTempLong'
    observeEvent(input$FilterDelete, {
      values$deltaTempLong <- deltaTempLongNoFilter()
    })
    
    #### UI ####

    #' Reactive variable to get start and end data of data set
    minMaxDatetime <- reactive({
      if (!is.null(input$file1)){
        req(input$setData)
      } 
      d = rawData()
      minDate = as.Date(d[which.min(as.POSIXct(d$datetime)),
                          "datetime"])
      maxDate = as.Date(d[which.max(as.POSIXct(d$datetime)),
                          "datetime"])
      return(c(minDate, maxDate))
    })
    
    #' Helper function to built Filter-UI (load or delete)
    filter_helper = function(input, output){
      if (!is.null(input$file1)){
        req(input$setData)
      }
      output = update.filter.ui(ui.output = output, ui.input = input)
      minMaxDatetime = minMaxDatetime()
      updateDateRangeInput(session, "daterange",
                           start = minMaxDatetime[1],
                           end = minMaxDatetime[2],
                           min = minMaxDatetime[1],
                           max = minMaxDatetime[2])
    }
    
    #' Eventlistener to built Filter-UI
    #' Calling helper function, same as delete filter
    observeEvent(input$LoadFilter, {
      filter_helper(input, output)
    })
    
    #' Eventlistener to built Filter-UI
    #' Calling helper function, same as load filter
    observeEvent(input$FilterDelete, {
      filter_helper(input, output)
    })
    
    
    #### Table outputs #####

    #' UI Table with raw data, wide-format
    #' (Data > Upload > Preview data)
    output$raw.wide <- DT::renderDataTable({
      return(rawData())
    }, options = list(scrollX = TRUE))
    
    #' UI Table with raw data, long-format 
    #' (Data > Upload > Preview data)
    output$raw.long <- DT::renderDataTable({
      return(deltaTempLongNoFilter() %>% 
               mutate_if(is.numeric, round, 3))
    }, options = list(scrollX = TRUE))
    
    #' UI Table with sensor data, i.e.
    #' sensor position, depth, area and circumference of ring
    #' (Data > Upload > Sensor settings)
    output$depth.table <- DT::renderDataTable({
      return(depths() %>%
               mutate_at(vars(3, 4, 5), round, 1) %>%
               select(-R) %>%
               `colnames<-` (c("Position", "Sensor R (cm)", "Area (cm²)",
                        "Circ. (cm)")))
    }, options = list(scrollX = TRUE))
    
    
    #### Text output ####

    #' UI Test output of remaining data points after filtering
    #' (Data > Filter > Subset data)    
    output$dataPoints <- renderText({
      n_diff = nrow(deltaTempLongNoFilter()) - nrow(deltaTempLong())
      paste(n_diff, " data points removed.")
    })
    
    #### Graphics ####

    #' Reactive variable holding the histogram-like 
    #' plot with (filtered) data
    filterPlot <- reactive({
      plot.histogram(data = deltaTempLong(), 
                     ui.input = input)
    })
    
    #' UI plot of filtered data
    #' (Data > Filter > Figures)
    output$filterPlot <- renderPlot({
      filterPlot()
    })
    
    #' Reactive variable holding the plot showing temperature
    #' differences
    deltaTfacetWrap <- reactive({
        plot.deltaTfacetWrap(data = deltaTempLong(), 
                             ui.input = input)
    })
    
    #' UI plot of temperature differences
    #' (Data > View > Figures)
    output$deltaTfacetWrap <- renderPlot({
      deltaTfacetWrap()
    })
    
    #' Reactive variable holding the plot showing customized
    #' temperature visualizations
    deltaTSingle <- reactive({
        plot.singleTemperature(data = deltaTempLong(),
                               ui.input = input)
    })
    
    #' UI of customized plot
    #' (Data > View > Figures)
    output$deltaTSingle <- renderPlot({
      deltaTSingle()
    })
    
    
    #### Buttons ####
    
    #' Eventlistener to save unfiltered, long-format data
    #' (Data > Upload > Preview data)
    observeEvent(input$save_dat_upl, {
        csvObject = deltaTempLongNoFilter()
        path = paste(projectPath(), 
                     "/csv-files/",
                     "deltaT_longFormat", sep = "")
        save.csv(path, csvObject, fileAppendix())
        
    })
    
    #' Eventlistener to save plot with temperature differences
    #' (Data > View > Figures)
    observeEvent(input$save.deltaTfacetWrap, {
        name = paste(projectPath(),
                     "/graphics/",
                     "deltaT_", input$rawPlot.facet, sep = "")
        obj = deltaTfacetWrap()
        save.figure(name, obj, figTitle(), fileAppendix(), input$figFor)
    })
    
    #' Eventlistener to save plot with customized temperatures
    #' (Data > View > Figures)
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

    #' Eventlistener to save filtered, long-format data
    #' (Data > Filter > Figures)
    observeEvent(input$save_dat_filter, {
      csvObject = values$deltaTempLong
      path = paste(projectPath(), 
                   "/csv-files/",
                   "deltaT_longFormat_filtered", sep = "")
      save.csv(path, csvObject, fileAppendix())
    })
    
    #' Eventlistener to save plot with filtered data
    #' (Data > Filter > Figures)
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
    
    #' UI radiobuttons to select sensor position
    #' Derives positions from filtered data set
    #' (K-value > Estimation > K-value estimation)
    output$kPositionSelect <- renderUI({
      positions = positions()
      pre_selected = positions[1]
      radioButtons("kPositionSelect", "Sensor position",
                   choices = positions,
                   selected = pre_selected, inline = T)
    })
    
    #' UI numeric inputs to define range in k-plots
    #' Only visible if 'fixed' range is enabled
    #' (K-value > Estimation > Control plots)
    output$xRangeSlider <- renderUI({
      data = deltaTempLong.depth()
      tagList(
        conditionalPanel(
          condition = "input.k1Plot_scales == 'TRUE'",
          fluidRow(
            column(6, numericInput("k1Plot.x.min", "Min. x-value", 
                                   round(min(data$dTsym.dTas), 2))),
            column(6, numericInput("k1Plot.x.max", "Max. x-value", 
                                   round(max(data$dTsym.dTas), 2))))
        ))
    })
    
    
    #### Variables ####
    
    #' Reactive variable holding a list with cleaned data,
    #' positive and negative k-value (K.dTas, K.dTsa) for a
    #' selected sensor position
    cleanedDataAndKvalues <- reactive({
      data = deltaTempLong() %>%
          filter(position == input$kPositionSelect)
      data = get.time.filtered.data(data = data,
                                    ui.input = input)
      return(clean.data.iteration(data))
    })
    
    #' Reactive variable holding k-values derived
    #' based on selected method for selected sensor
    #' position
    kValue <- reactive({
      return(get.kByMethod(data = deltaTempLong(),
                           ui.input = input))
    })

    #' Reactive variable holding k-values derived
    #' based on selected method (closest and regression)
    #' for all sensor positions, shown in tables
    kComplete <- reactive({
      return(get.kByMethodAll(deltaTempLong(),
                              ui.input = input))
    })
    
    #' Reactive variable holding k-values derived
    #' from csv-input for all sensor positions
    kFromCsv <- reactive({
      req(input$file2)
      return(get.csvKvalues(ui.input = input))
    })
    
    #' Reactive variable holding a helper to check 
    #' if any k-values have been set
    click <- reactive(({
      click = input$setK[1] + input$setKfromCsv[1] + input$setKfromRegression[1] +
        input$setKfromClosest[1]
    }))
    
    #### Store and display selected k-values ####
    
    #' Reactive value: assign empty placeholder to 
    #' store selected k-values
    values <- reactiveValues(kvalues = NULL)  
    
    #' Reactive helper function to clear reactive kvalues
    emptyKvalues = reactive({
      values$kvalues <-  data.frame(position = positions(),  
                                    method = rep(NA),
                                    k = rep(NA))
    })
    
    #' Eventlistener to set/ store k-value as reactive value
    #' K-value is stored in reactive value
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setK, {  
      click = click()
      if (click == 1 && is.null(input$file2)){
        emptyKvalues()
      }
      values$kvalues[values$kvalues$position == input$kPositionSelect, 2:3] <- cbind(
        method = as.character(input$kMethod),
        k = round(kValue(), 3))
    })

    #' Eventlistener to store k-values from csv upload
    #' as reactive values
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setKfromCsv, {
      emptyKvalues()
      values = fill.k.table(method = "csv",
                            k.data = kFromCsv(), 
                            ui.input = input, 
                            reactive.value = values)
    })
    
    #' Eventlistener to store k-values from automatic 
    #' regression as reactive values
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setKfromRegression, {
      emptyKvalues()
      values = fill.k.table(method = "regression",
                            k.data = kComplete()$regression %>% round(., 3), 
                            ui.input = input, 
                            reactive.value = values)
    })
    
    #' Eventlistener to store k-values from closest 
    #' no-flow estimate as reactive values
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setKfromClosest, {
      emptyKvalues()
      values = fill.k.table(method = "closest",
                            k.data = kComplete()$closest %>% round(., 3), 
                            ui.input = input, 
                            reactive.value = values)
    })

    #### Text outputs ####

    #' UI text output of current k-value,
    #' depending on selected position and method
    #' (K-value > Estimation > K-value estimation)
    output$kCurrent <- renderPrint({ 
        paste("K-value", round(kValue(), 3))
    })
    
    
    #### Table outputs ####
    
    #' UI table output of selected k-values
    #' (K-value > Estimation > K-value estimation > Selected)
    output$kSelected <- DT::renderDataTable({  
      return(values$kvalues)
    }, options = list(scrollX = TRUE))
    
    #' UI table output of auto. regression k-values
    #' (K-value > Estimation > K-value estimation > Regression)
    output$kRegression <- DT::renderDataTable({  
      return(kComplete()$regression  %>% round(., 2))
    }, options = list(scrollX = TRUE))
    
    #' UI table output of closest zero-flow k-values
    #' (K-value > Estimation > K-value estimation > Closest)
    output$kClosest <- DT::renderDataTable({
      return(kComplete()$closest %>% round(., 2))
    }, options = list(scrollX = TRUE))

    #' UI table output of uploaded k-values
    #' (K-value > Estimation > K-value estimation > Read csv)
    output$uploadedKvalues <- DT::renderDataTable({ 
      return(kFromCsv())
    }, options = list(scrollX = TRUE))
  
    
    #### Graphics ####
    
    #' Reactive variable holding the nighttime plot
    kNightTime <- reactive({
      plot.nighttime(data.complete = deltaTempLong.depth())
    })
    
    #' UI output of nighttime plot
    #' (K-value > Estimation > Control plots > Night time)
    output$kNightTimePlot <- renderPlot({ kNightTime() })
    
    #' Reactive variable holding the k-diagram
    kplot1 <- reactive({
      plot.kEst1(data.complete = deltaTempLong.depth(),
                 data.adj = cleanedDataAndKvalues()[[1]],
                 ui.input = input)
    })
    
    #' UI output of K-diagram
    #' (K-value > Estimation > Control plots > K-diagram)
    output$kvaluePlot1 <- renderPlot({ kplot1() })
    
    #' Reactive variable holding the control-diagram 1
    kplot2 <- reactive({
      plot.kEst2(data.complete = deltaTempLong.depth(),
                 data.adj =cleanedDataAndKvalues()[[1]],
                 k = kValue(),
                 ui.input = input)
    })
    
    #' UI output of Control-diagram 1
    #' (K-value > Estimation > Control plots > Control-diagram 1)
    output$kvaluePlot2 <- renderPlot({ kplot2() })
    
    #' Reactive variable holding the control-diagram 2
    kplot3 <- reactive({
        plot.kEst3(data.complete = deltaTempLong.depth(), 
                   data.adj = cleanedDataAndKvalues()[[1]],
                   k = kValue(),
                   ui.input = input)
    })
    
    #' UI output of Control-diagram 2
    #' (K-value > Estimation > Control plots > Control-diagram 2)
    output$kvaluePlot3 <- renderPlot({ kplot3() })
    
    #### Buttons ####
    
    #' Eventlistener to save selected k-values as csv 
    #' (K-value > Estimation > K-value estimation > Selected)
    observeEvent(input$save.kValues, {
      path = paste(projectPath(), 
                   "/csv-files", sep = "")
      csvObject = values$kvalues
      save.csv(paste(path, "/k-values", sep = ""), 
               csvObject, fileAppendix())
    })
    
    #' Eventlistener to save k-diagrams
    #' (K-value > Estimation > Control plots)
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
      kValues = values$kvalues
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
