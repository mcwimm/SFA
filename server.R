shinyServer(function(input, output, session) {
  
    ########################
    ### PROJECT SETTINGS ###
    ########################
    
    #### Variables ####
  
    #' Shiny function that returns 'available volumes on the system'
    volumes = getVolumes()
    
    #' Variable holding possible root directories
    roots = c('working directory' = ".",
              system = volumes())
    
    #' Shiny function to 'to navigate the filesystem'
    folderInput1 <- shinyDirChoose(input, 'folder',
                                   roots = roots, 
                                   filetypes = c('', 'txt'))
    
    
    
    #' Reactive variable holding the current project path
    #' If not project is selected it returns the root directory
    projectPath <- reactive({
      if (!isTruthy(input$folder)){
        roots[[1]]
      } else {
        parseDirPath(roots = roots, selection = input$folder)      
      }
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
    
    #' Reactive variable holding colors to be used in 
    #' all plots with gradient color scale
    #' Can be defined in UI
    gradientcolors_react = reactive({
      return(get.gradientcolors(ui.input = input))
    })
    
    #' Global function (accessible from other scripts) 
    #' that returns 2 colors
    gradientcolors <<- function(){
      col = gradientcolors_react()
      return(col)
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
        projectPath()
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
          projectName()
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
  
    #' Trigger to update reactive data when 'Use data' 
    #' button is pressed
    #' Updates data for the whole App
    observeEvent(input$setData, {
      values$deltaTempLong <- deltaTempLongNoFilter()
      # Reset selected K values (empty table)
      emptyKvalues()
    })
    
    #' Reactive variable holding long-format data for
    #' specific UI-selected sensor position
    deltaTempLong.depth <- reactive({
      req(input$kPositionSelect)
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
      return(rawData() %>% 
               mutate(dTime = round(dTime, 2)))
    }, options = list(scrollX = TRUE, searching = F))
    
    #' UI Table with raw data, long-format 
    #' (Data > Upload > Preview data)
    output$raw.long <- DT::renderDataTable({
      return(deltaTempLongNoFilter() %>% 
               mutate_if(is.numeric, round, 3))
    }, options = list(scrollX = TRUE, searching = F))
    
    #' UI Table with sensor data, i.e.
    #' sensor position, depth, area and circumference of ring
    #' (Data > Upload > Sensor settings)
    output$depth.table <- DT::renderDataTable({
      return(depths() %>%
               mutate_at(vars(3, 4, 5), round, 1) %>%
               select(-R) %>%
               `colnames<-` (c("Position", "Sensor R (cm)", "Area (cm²)",
                        "Circ. (cm)")))
    }, options = list(scrollX = TRUE, searching = F))
    
    
    #### Text output ####

    #' UI Test output of remaining data points after filtering
    #' (Data > Filter > Subset data)    
    output$dataPoints <- renderText({
      filtered_data = deltaTempLong()
      n_diff = nrow(deltaTempLongNoFilter()) - nrow(filtered_data)
      if (any(is.na(filtered_data))){
        paste("Data set contains NA values. <br/><br/>", 
                    n_diff, " data points removed.")
      } else {
        paste(n_diff, " data points removed.")
      }
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
    

    ##### Custom View ####
    
    #' Assign empty reactive value holding ui inputs for
    #' customized figure
    values <- reactiveValues(plotSettings = NULL)
    
    #' Eventlistener assigning ui inputs to customize figure
    #' to reactive value
    observeEvent(input$renderPlot, {
      values$plotSettings <- get.customizedPlotSettings(ui.input = input)
    })
    
    #' Reactive variable holding ui inputs to customize figure
    plotSettings <- reactive({
      return(values$plotSettings)
    })
    
    #' Reactive variable holding the plot showing customized
    #' temperature visualizations
    custumPlot <- reactive({
      req(input$renderPlot)
      plot.customTemperature(data = deltaTempLong(),
                             ui.input.processed = plotSettings())
    })

    #' #' UI of customized plot
    #' #' (Data > View > Figure)
    output$custumPlot <- renderPlot({
      if (input$renderPlot == 0){
        plot.emptyMessage("Customize your figure (settings).")
      } else {
        custumPlot()
      }
    })
    
    
    #### Buttons ####
    
    #' Eventlistener to save unfiltered, long-format data
    #' (Data > Upload > Preview data)
    observeEvent(input$save_dat_upl, {
        save.csv(path = projectPath(), 
                 name = "dTemp",
                 csvObject = deltaTempLongNoFilter(), 
                 ui.input = input)
        
    })
    
    #' Eventlistener to save plot with customized temperatures
    #' (Data > View > Figure)
    observeEvent(input$save.custumPlot, {
      name = paste("dT",
                   input$rawPlot.xcol, 
                   input$rawPlot.ycol, 
                   input$rawPlot.col,  
                   input$rawPlot.shape,
                   input$rawPlot.facet, sep = "_")
      save.figure(path = projectPath(),
                  name = name,
                  plotObject = custumPlot(), 
                  ui.input = input)
    })

    #' Eventlistener to save filtered, long-format data
    #' (Data > Filter > Figures)
    observeEvent(input$save_dat_filter, {
      save.csv(path = projectPath(), 
               name = "dTemp_filtered",
               csvObject = values$deltaTempLong, 
               ui.input = input)
    })
    
    #' Eventlistener to save plot with filtered data
    #' (Data > Filter > Figures)
    observeEvent(input$save_dat_filter_fig, {
      name = paste("dT_filtered",
                   as.character(input$filterPlot_type),
                   as.character(input$filterPlot_X),
                   as.character(input$filterPlot_col), sep = "_")
      save.figure(path = projectPath(),
                  name = name,
                  plotObject = filterPlot(), 
                  ui.input = input)
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
      radioButtons("kPositionSelect", "Thermometer position",
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
                                   round(min(data$dTsym.dTas, na.rm = T), 2))),
            column(6, numericInput("k1Plot.x.max", "Max. x-value", 
                                   round(max(data$dTsym.dTas, na.rm = T), 2))))
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
    #' based on selected method (zero-flow and regression)
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
        input$setKfromZeroFlow[1]
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
      if (click == 1){
        emptyKvalues()
      }
      if (input$dTimeFilter & input$kMethod == "regression"){
        method_name = paste("regression", " (",
                            input$kRegressionTime.start, " - ",
                            input$kRegressionTime.end, ")",
                            sep = "")
      } else {
        method_name = gsub(".", "-", input$kMethod, fixed = TRUE)
      }
      values$kvalues[values$kvalues$position == input$kPositionSelect, 2:3] <- cbind(
        method = as.character(method_name),
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
    
    #' Eventlistener to store k-values from zero-flow 
    #' no-flow estimate as reactive values
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setKfromZeroFlow, {
      emptyKvalues()
      values = fill.k.table(method = "no.flow",
                            k.data = kComplete()$no.flow %>% round(., 3), 
                            ui.input = input, 
                            reactive.value = values)
    })

    #### Text outputs ####

    #' UI text output of current k-value,
    #' depending on selected position and method
    #' (K-value > Estimation > K-value estimation)
    output$kCurrent <- renderPrint({ 
      req(input$kPositionSelect)
      paste("K-value", round(kValue(), 3))
    })
    
    
    #### Table outputs ####
    
    #' UI table output of selected k-values
    #' (K-value > Estimation > K-value estimation > Selected)
    output$kSelected <- DT::renderDataTable({  
      return(values$kvalues)
    }, options = list(scrollX = TRUE, dom = 't'))
    
    #' UI table output of auto. regression k-values
    #' (K-value > Estimation > K-value estimation > Regression)
    output$kRegression <- DT::renderDataTable({  
      return(kComplete()$regression  %>% round(., 2))
    }, options = list(scrollX = TRUE, dom = 't'))
    
    #' UI table output of closest zero-flow k-values
    #' (K-value > Estimation > K-value estimation > Zero-flow)
    output$kZeroFlow <- DT::renderDataTable({
      return(kComplete()$no.flow %>% round(., 2))
    }, options = list(scrollX = TRUE, dom = 't'))

    #' UI table output of uploaded k-values
    #' (K-value > Estimation > K-value estimation > Read csv)
    output$uploadedKvalues <- DT::renderDataTable({ 
      return(kFromCsv())
    }, options = list(scrollX = TRUE, dom = 't'))
  
    
    #### Graphics ####
    
    #' Reactive variable holding the diurnal flow plot
    #' ggplot warnings are suppressed
    kplotDiurnalFlow <- reactive({
      plot.nighttime(data.complete = deltaTempLong.depth())
    })
    
    #' UI output of diurnal flow plot
    #' (K-value > Estimation > Control plots > Diurnal flow)
    output$kDiurnalPlot <- renderPlot({ kplotDiurnalFlow() })
    
    #' Reactive variable holding the k-diagram
    kplot1 <- reactive({
      suppressWarnings(print(
        plot.kEst1(data.complete = deltaTempLong.depth(),
                   data.adj = cleanedDataAndKvalues()[[1]],
                   ui.input = input)
      ))
    })
    
    #' UI output of K-diagram
    #' (K-value > Estimation > Control plots > K-diagram)
    output$kvaluePlot1 <- renderPlot({ kplot1() })
    
    #' Reactive variable holding the control-diagram 1
    #' ggplot warnings are suppressed
    kplot2 <- reactive({
      # suppressWarnings(print(
        plot.kEst2(data.complete = deltaTempLong.depth(),
                 data.adj =cleanedDataAndKvalues()[[1]],
                 k = kValue(),
                 ui.input = input)
      # ))
    })
    
    #' UI output of Control-diagram 1
    #' (K-value > Estimation > Control plots > Control-diagram 1)
    output$kvaluePlot2 <- renderPlot({ kplot2() })
    
    #' Reactive variable holding the control-diagram 2
    #' ggplot warnings are suppressed
    kplot3 <- reactive({
      suppressWarnings(print(
        plot.kEst3(data.complete = deltaTempLong.depth(), 
                   data.adj = cleanedDataAndKvalues()[[1]],
                   k = kValue(),
                   ui.input = input)
      ))
    })
    
    #' UI output of Control-diagram 2
    #' (K-value > Estimation > Control plots > Control-diagram 2)
    output$kvaluePlot3 <- renderPlot({ kplot3() })
    
    #### Buttons ####
    
    #' Eventlistener to save selected k-values as csv 
    #' (K-value > Estimation > K-value estimation > Selected)
    observeEvent(input$save.kValues, {
      csvObject = values$kvalues
      save.csv(path = projectPath(), 
               name = "K_values",
               csvObject = csvObject, 
               ui.input = input)
    })
    
    #' Eventlistener to save k-diagrams
    #' (K-value > Estimation > Control plots) 
    observeEvent(input$save.kPlots, {
      appendix = input$kPositionSelect
      if (input$dTimeFilter & input$kMethod == "regression"){
        appendix = paste(appendix, "time",
                          input$kRegressionTime.start,
                          input$kRegressionTime.end, 
                          sep = "_")
      }
      save.figure(path = projectPath(),
                  name = paste("k-diagram_sensor", appendix, sep = "_"),
                  plotObject = kplot1(),
                  ui.input = input)
      save.figure(path = projectPath(),
                  name = paste("k-control-1_sensor", appendix, sep = "_"),
                  plotObject = kplot2(),
                  ui.input = input)
      save.figure(path = projectPath(),
                  name = paste("k-control-2_sensor", appendix, sep = "_"),
                  plotObject = kplot3(),
                  ui.input = input)
      
    })

    ####################
    ##### SAP FLOW  ####
    ####################
    #### Variables ####
    
    #' Reactive variable holding sap wood depth
    sapWoodDepth <- reactive({
      return(get.sapWoodDepth(ui.input = input))
    })
    
    #' Reactive variable holding sap flow density
    #' based on k-values
    sapFlowDens <- reactive({
      data = add.k2data(data = deltaTempLong(),
                        values = values)
      return(get.sapFlowDensity(method = "HFD",
                                data = data,
                                sapWoodDepth = sapWoodDepth(),
                                ui.input = input))
    })
    
    #' Reactive variable holding distances between
    #' sensors
    sensor.dist <- reactive({
      return(get.sensorDistance(ui.input = input))
    })
    
    #' Reactive variable holding sap flow rates
    #' for all methods
    sapFlow <- reactive({
      return(get.sapFlow(data = sapFlowDens(),
                         depths = depths(), 
                         sapWoodDepth = sapWoodDepth(),
                         ui.input = input))
    })
    
    #' Reactive variable holding daily tree water 
    #' use in kg per h and kg per day
    #' for selected method
    treeWaterUse <- reactive({
      if (click() > 0){
        get.treeWaterUseByMethod(data = sapFlow(),
                                 ui.input = input)
      } else {
        return(data.frame(x = "No k-values have been set yet."))
      }
    })
    
    #### Graphics ####
    
    ##### Sap Flow Metrics Diurnal Pattern #####
    
    #' Helper function that catches each trigger element in sap flow panel
    triggerSfPlotUpdate = reactive({
      return(!is.null(input$sf_y_axis)| 
               !is.null(input$sf_facet_scales) |
               !is.null(input$sf_facet_column) |
               !is.null(input$sf_facet_col_nums) |
               input$sf_style != "sf_grouped" | 
               input$sf_grouped_go |
               click() != 0 |
               sapWoodDepth() != 0)
    })
    
    #' Reactive variable holding figure of sap flow density
    #' If group be thermocouple checkbox is activated evaluation stops 
    #' until button is pressed
    sapFlowMetricPlot <- eventReactive(triggerSfPlotUpdate(), {
        if (input$sf_y_axis == "SFI"){
          plot.sf.helper(data = deltaTempLong(),
                      ui.input = input)
        } else {
          if (click() == 0 && is.null(input$file2)){
            plot.emptyMessage(message = "No k-values have been set yet.")
          } else {
            plot.sf.helper(data = sapFlowDens(),
                        ui.input = input)
          }
        }
    })
    
    #' Eventlistener to show figure of sap flow density
    #' (Sap Flow > Sap Flow Density > Figure > Diurnal pattern)
    output$sapFlowMetric <- renderPlot({
      sapFlowMetricPlot()
    })
    
    #' Eventlistener to save sap flow density plot
    #' (Sap Flow > Sap Flow Density > Figures > Diurnal pattern)
    observeEvent(input$save.sapFlowMetricPlot, {
      save.figure(path = projectPath(),
                  name = input$sf_y_axis, 
                  plotObject = sapFlowMetricPlot(), 
                  ui.input = input)
    })
    
    #' Eventlistener to save sap flow density plot
    #' vertical profile
    #' (Sap Flow > Sap Flow Density > Figures > Sensor profile)
    observeEvent(input$save.sapFlowMetrics, {
      save.csv(path = projectPath(), 
               name = input$sf_y_axis,
               csvObject = sapFlowDens(), 
               ui.input = input)
    })
    
    ##### Sap Flow Metrics Radial Profile #####
    
    #' Reactive variable holding figure of sap flow metrics
    #' radial profile, represented as boxplots
    sapFlowMetricPlot.RadialProfile = eventReactive(triggerSfPlotUpdate(), {
      if (input$sf_y_axis == "SFI"){
        plot.sf.helper(data = deltaTempLong(),
                    ui.input = input,
                    radial.profile = TRUE)
      } else {
        if (click() == 0 && is.null(input$file2)){
          plot.emptyMessage(message = "No k-values have been set yet.")
        } else {
          plot.sf.helper(data = sapFlowDens(),
                      ui.input = input,
                      radial.profile = TRUE)
        }
      }
    })
    
    #' Eventlistener to show figure of sap flow density
    #' vertical profile
    #' (Sap Flow > Sap Flow Density > Figures > Sensor profile)
    output$sapFlowMetric.RadialProfile <- renderPlot({
      sapFlowMetricPlot.RadialProfile()
    })
    
    #' Eventlistener to save sap flow density plot
    #' (Sap Flow > Sap Flow Density > Figures > Sensor profile)
    observeEvent(input$save.sapFlowMetric.RadialProfile, {
      save.figure(path = projectPath(),
                  name = paste(input$sf_y_axis, "r-profil", sep = "_"),
                  plotObject = sapFlowMetricPlot.RadialProfile(), 
                  ui.input = input)
      
    })

    
    ##### Sap Flow Rate #####
    ###### Diurnal Pattern ######
    
    #' Reactive variable holding figure of sap flow rate
    #' for selected methods
    sapFlowTreePlot <- reactive({
      if (click() == 0 && is.null(input$file2)){
        plot.emptyMessage(message = "No k-values have been set yet.")
      } else {
        if (sapWoodDepth() == 0){
          plot.emptyMessage(message = "Wood properties are missing (see 'Project settings')")
        } else {
          plot.sapFlowRate(data = sapFlow(), 
                           ui.input = input)
        }}
    })
    
    #' Eventlistener to show figure of sap flow rate
    #' (Sap Flow > Sap Flow > Figures > Diurnal pattern)
    output$sapFlowTree <- renderPlot({
      sapFlowTreePlot()
    })
    
    #' Eventlistener to save sap flow rate figure
    #' (Sap Flow > Sap Flow > Figures > Diurnal pattern)
    observeEvent(input$save.sapFlowTree, {
      save.figure(path = projectPath(),
                  name = "SapFlow",
                  plotObject = sapFlowTreePlot(), 
                  ui.input = input)
    })
    
    #' Eventlistener to save sap flow rate data as csv
    #' (Sap Flow > Sap Flow > Figures > Diurnal pattern)
    observeEvent(input$save.SapFlowCsv, {
      save.csv(path = projectPath(), 
               name = "SapFlow",
               csvObject = sapFlow(), 
               ui.input = input)
    })
    
    ###### Daily Balance ######
    
    #' Reactive variable holding figure of daily water balance
    #' for selected methods
    TWUbarplot <- reactive({
      if (click() == 0 && is.null(input$file2)){
        plot.emptyMessage(message = "No k-values have been set yet.")
      } else {
        if (sapWoodDepth() == 0){
          plot.emptyMessage(message = "Wood properties are missing (see 'Project settings')")
        } else {
          plot.sapFlowDay(data = sapFlow(), 
                          ui.input = input)
        }}
    })
    
    #' Eventlistener to show figure of daily water balance
    #' (Sap Flow > Sap Flow > Figures > Daily balance)
    output$TWUbar <- renderPlot({
      TWUbarplot()
    })
    
    #' Eventlistener to save daily water balance
    #' (Sap Flow > Sap Flow > Figures > Daily balance)
    observeEvent(input$save.TWUbarplot, {
      save.figure(path = projectPath(),
                  name = "SapFlow_Balance",
                  plotObject = TWUbarplot(), 
                  ui.input = input)
    })
    
    ###### Radial profile #####
    
    #' Reactive variable holding figure of daily water balance
    #' for selected methods
    TWUradialprofilePlot <- reactive({
      if (click() == 0 && is.null(input$file2)){
        plot.emptyMessage(message = "No k-values have been set yet.")
      } else {
        if (sapWoodDepth() == 0){
          plot.emptyMessage(message = "Wood properties are missing (see 'Project settings')")
        } else {
          plot.twu.radialprofile(data = sapFlow(), 
                                 ui.input = input)
        }}
    })
    
    #' Eventlistener to show figure of daily water balance
    #' (Sap Flow > Sap Flow > Figures > Daily balance)
    output$TWUradialprofile <- renderPlot({
      TWUradialprofilePlot()
    })
    
    #' Eventlistener to save daily water balance
    #' (Sap Flow > Sap Flow > Figures > Daily balance)
    observeEvent(input$save.TWUradialprofile, {
      save.figure(path = projectPath(),
                  name = "TWU_r-profile",
                  plotObject = TWUradialprofilePlot(), 
                  ui.input = input)
    })
    
    
    #### Table ####
    
    #' UI-Table with daily tree water use
    output$TWUtable <- DT::renderDataTable({ 
      if (sapWoodDepth() == 0){
        data.frame('.' = "Wood properties are missing (see 'Project settings')")
      } else {
        treeWaterUse()
      }
    }, options = list(scrollX = TRUE, dom = 't'))
    
    #' Eventlistener to save daily tree water use as csv
    #' (Sap Flow > Sap Flow > Tree water use)
    observeEvent(input$save.TWUCsv, {
      save.csv(path = projectPath(), 
               name = "TWU",
               csvObject = treeWaterUse(), 
               ui.input = input)
    })
})
