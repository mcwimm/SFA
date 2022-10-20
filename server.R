# Define file upload limit, now 15 MB
options(shiny.maxRequestSize = 15*1024^2)

shinyServer(function(input, output, session) {
    ######################
    ### Error messages ###
    ######################
  
    message.fail.upload = "An error occured. Please check your upload settings (e.g. number of lines skipped) and required column names."
    message.no.k = "No k-values have been set yet."
    message.no.sapflow = "No sap flow data available. \nMake sure wood and sensor properties \nare entered correctly (see Settings)."
  
    #' Function to return table with warning message
    tab.with.message = function(message,
                                col = "#E56855", # theme red
                                background = "#cccccc" # theme grey
    ) {
      m = matrix(data = c(message))
      return(
        datatable(m, options = list(scrollX = TRUE, dom = 't'), colnames = NULL) %>%
          formatStyle(
            1,
            color = col,
            backgroundColor = background,
            fontWeight = 'bold'
          )
      )
    }
    
    
    ########################
    ### PROJECT SETTINGS ###
    ########################
    
    #### Variables ####
  
    #' Shiny function that returns 'available volumes on the system'
    volumes = getVolumes()
    
    #' Variable holding possible root directories
    roots = c('working directory' = getwd(),
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
    
    #' Set default theme
    theme_set(theme_bw())
    
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
      len = length(col)
      return(col[(len-N):len])
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
      #req(input$figTheme)
      theme_set(plot_theme())
      NULL
    })
    
    #' Show project path in Project Settings > Project
    #' if a project folder is selected
    output$prjDir <- renderPrint({
        cat(projectPath())
    })
    
    #' Show string as default
    output$prjName <- renderPrint({
      cat("No project chosen")
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
          cat(projectName())
      })
    })
    
    #### Thermometer positions ####
    
    #' Reactive variable holding thermometer positions
    #' (a vector numbering the thermometers) derived from input file
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
    
    #' Reactive variable holding depth of each thermometer positions
    #' in cm
    depths <- reactive({
      if (!is.null(input$file1)){
        req(input$setData)
      }
      out = try(
        {
          
          update.depths(ui.input = input,
                        positions = positions(),
                        swd = sapWoodDepth())  
        }
      )
      if (is.character(out)){
        return(NULL)
      } else {
        return(out)
      }
    })
    
    #' Table with thermometer data, i.e.
    #' thermometer position, depth, area and circumference of ring
    #' (Settings > Measuring environment)
    get.depths.table <- reactive({
      rawData = rawData()
      
      # Conditions to determine whether processed data contains relevant
      # wood properties
      # If true, show them
      cond1 = input$inputType == "HFD_processed_read"
      cond2 = input$inputType == "HFD_processed_write" &
        sapWoodDepth() == 0
      cond3 = all(c("position", "R", "Aring", "Cring") %in% colnames(rawData))
      
      if ((cond1 | cond2) & cond3) {
        return(
          rawData %>%
            distinct(position, R, Aring, Cring) %>%
            select(position, R, Aring, Cring) %>%
            mutate_at(vars(2, 3, 4), round, 1) %>%
            `colnames<-` (
              c("Position", "Thermometer R (cm)", "Area (cm²)",
                "Circ. (cm)")
            )
        )
      } else {
        depths = depths()
        if (is.null(depths) | sum(depths$R) == 0) {
          return(
            tab.with.message(
              message = "Specify wood and sensor properties. Make sure the number of inputs corresponds to your data."
            )
          )
        } else {
          return(
            depths %>%
              select(-R) %>%
              mutate_at(vars(2), round, 2) %>%
              mutate_at(vars(3, 4), round, 1) %>%
              `colnames<-` (
                c(
                  "Position",
                  "Thermometer R (cm)",
                  "Area (cm²)",
                  "Circ. (cm)"
                )
              )
          )

        }
      }
    })

    #' UI depths table
    output$depth.table <- DT::renderDataTable(rownames = FALSE, {
      return(get.depths.table())
    }, options = list(scrollX = TRUE, dom = 't'))

    output$depth.table.info <- renderText({
      depths = get.depths.table()
      note = ""
      if (!is.null(ncol(depths))){
        if (min(depths[, 2]) < 0){
          note = paste(
            note,
            "<b>Note:</b> negative values for 'Thermometer R' indicate that the sensor needles cross the center of the
        tree (i.e. needle length > diameter / 2 - barkthickness) and the respective thermometer positions are on the
        opposite side of the tree. ", sep="<br/>")
        }
        needle_cover = max(depths[, 2]) - min(depths[, 2])
        if (needle_cover > (2*get.rxy(ui.input = input))){
          note = paste(
            note,
            "<b>Note:</b> the sensors needles seem to be longer than the stem diameter.", sep="<br/>")
        }
      }
      return(note)
    })
    
    #' Eventlistener to save thermometer depth table
    #' (Project Settings > Measuring environment)
    observeEvent(input$save.sensor_props, {
       save.csv(path = projectPath(), 
                name = "sensor_props",
                csvObject = get.depths.table(), 
                ui.input = input)
    })
    
    
    ############
    ### DATA ###
    ############
    
    #### Variables ####

    #' Reactive variable holding raw sap flow data
    #' If no data set is defined, use default data set
    rawData <- reactive({
      if (is.null(input$file1)){
        defaultData = "./data/default_Avicennia_g.csv"
        print("Default data")
        data = get.temperatures.HFD(defaultData,
                                    sep = ";",
                                    skip = 0)
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
      if (input$inputType == "HFD_processed_read" |
          input$inputType == "HFD_processed_write"){
        d = data
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
    #' specific UI-selected thermometer position
    deltaTempLong.depth <- reactive({
      req(input$kPositionSelect)
      deltaTempLong() %>% 
        filter(position == input$kPositionSelect)
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
    output$raw.wide <- DT::renderDataTable(rownames = FALSE, {
      rawData = rawData()
      if ("dTime" %in% colnames(rawData)){
        rawDataTable = rawData %>% 
               mutate(dTime = round(dTime, 2))
      } else {
        rawDataTable = tab.with.message(message.fail.upload)
      }
      return(rawDataTable)
    }, options = list(scrollX = TRUE,
                      scrollY = "400px",
                      dom = "t")) 
    
    
    #' UI Table with raw data, long-format 
    #' (Data > Upload > Preview data)
    output$raw.long <- DT::renderDataTable(rownames = FALSE, {
      an.error.occured = FALSE
      tryCatch({
        tab  = deltaTempLongNoFilter() %>%
                           mutate_if(is.numeric, round, 3) 
      },
      error = function(e) {
        an.error.occured <<- TRUE
      })
      if (an.error.occured) {
        tab = tab.with.message(message.fail.upload)
      }
      return(tab)
    }, options = list(scrollX = TRUE,
                      scrollY = "400px",
                      dom = "t")) 
    
    #### Text output ####

    #' UI Text output of remaining data points after filtering
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
    
    #' UI radio buttons to select thermometer position
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
    #' selected thermometer position
    cleanedDataAndKvalues <- reactive({
      data = deltaTempLong() %>%
          filter(position == input$kPositionSelect)
      data = get.time.filtered.data(data = data,
                                    ui.input = input)
      data = get.dTratio.filtered.data(data = data,
                                    ui.input = input)
      return(clean.data.iteration(data))
    })
    
    #' Reactive variable holding k-values derived
    #' based on selected method for selected thermometer
    #' position
    kValue <- reactive({
      return(get.kByMethod(data = deltaTempLong(),
                           ui.input = input))
    })
    

    #' Reactive variable holding k-values derived
    #' based on selected method (zero-flow and regression)
    #' for all thermometer positions, shown in tables
    kComplete <- reactive({
      return(get.kByMethodAll(deltaTempLong(),
                              ui.input = input))
    })
    
    #' Reactive variable holding k-values derived
    #' from csv-input for all thermometer positions
    kFromCsv <- reactive({
      req(input$file2)
      return(get.csvKvalues(ui.input = input))
    })
    
    #' Reactive variable holding a helper to check 
    #' if any k-values have been set
    click <- reactive({
      click = input$setK[1] + input$setKfromCsv[1] + input$setKfromRegression[1] +
        input$setKfromZeroFlow[1]
      # check if values$kvalues is filled due to upload of processed data
      if ((input$inputType == "HFD_processed_read" | 
           input$inputType == "HFD_processed_write") & 
          !is.null(values$kvalues)){
        click = click + 1
      }
      return(click)
    })
    
    #### Store and display selected k-values ####
    
    #' Reactive value: assign empty placeholder to 
    #' store selected k-values
    values <- reactiveValues(kvalues = NULL)  
    
    #' Reactive helper function to clear reactive kvalues
    emptyKvalues = reactive({
      values$kvalues <-  data.frame(position = positions(),  
                                    method = rep(NA),
                                    k = rep(NA))
      if (input$inputType == "HFD_processed_read" | 
          input$inputType == "HFD_processed_write"){
          values$kvalues <-  deltaTempLong() %>%
                                mutate(method = "HFD_processed") %>% 
                                distinct(position, method, k) %>% 
                                select(position, method, k)
      }
    })
    
    #' Eventlistener to set/ store k-value as reactive value
    #' K-value is stored in reactive value
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setK, {  
      click = click()
      if (click == 1){
        emptyKvalues()
      }
      if (input$kMethod == "regression"){
        method_name = method_name_reg(ui.input = input)
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
                            k.data = kComplete()$regression %>% mutate_if(is.numeric, round, 3), 
                            ui.input = input, 
                            reactive.value = values)
    })
    
    #' Eventlistener to store k-values from zero-flow 
    #' no-flow estimate as reactive values
    #' (K-value > Estimation > K-value estimation)
    observeEvent(input$setKfromZeroFlow, {
      emptyKvalues()
      values = fill.k.table(method = "no.flow",
                            k.data = kComplete()$no.flow %>% mutate_if(is.numeric, round, 3), 
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
    output$kSelected <- DT::renderDataTable(rownames = FALSE, {  
      return(values$kvalues)
    }, options = list(scrollX = TRUE, dom = 't'))
    
    #' UI table output of auto. regression k-values
    #' (K-value > Estimation > K-value estimation > Regression)
    output$kRegression <- DT::renderDataTable(rownames = FALSE, {  
      return(kComplete()$regression %>% mutate_if(is.numeric, round, 2))
    }, options = list(scrollX = TRUE, dom = 't'))
    
    #' UI table output of closest zero-flow k-values
    #' (K-value > Estimation > K-value estimation > Zero-flow)
    output$kZeroFlow <- DT::renderDataTable(rownames = FALSE, {
      return(kComplete()$no.flow %>% mutate_if(is.numeric, round, 2))
    }, options = list(scrollX = TRUE, dom = 't'))

    #' UI table output of uploaded k-values
    #' (K-value > Estimation > K-value estimation > Read csv)
    output$uploadedKvalues <- DT::renderDataTable(rownames = FALSE, { 
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
                   k = kValue(),
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
      if (input$kMethod == "regression"){
        appendix = paste(appendix, method_name_reg(ui.input = input), sep = "_")
      } else {
        appendix = paste(appendix, input$kMethod, sep = "_")
      }

      # Replace characters in file name 
      appendix = str_replace_all(appendix, pattern = ",", replacement = "_")
      appendix = str_replace_all(appendix, pattern = ":", replacement = "_")
      appendix = str_replace_all(appendix, pattern = " ", replacement = "")
      appendix = str_replace_all(appendix, pattern = "\\.", replacement = "_")
      
      save.figure(path = projectPath(),
                  name = paste("k-diagram_position", "_", appendix, sep = ""),
                  plotObject = kplot1(),
                  ui.input = input)
      save.figure(path = projectPath(),
                  name = paste("k-control-1_position", "_", appendix, sep = ""),
                  plotObject = kplot2(),
                  ui.input = input)
      save.figure(path = projectPath(),
                  name = paste("k-control-2_position", "_", appendix, sep = ""),
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
      data = deltaTempLong()
      # Only calculate SFD if not in read mode
      if (input$inputType == "HFD_processed_read" & !is.null(values$kvalues)){
        return(data)
      } else {
        if (input$inputType == "HFD_processed_write"){
          # if processed file is uploaded delete existing k values
          data$k = NULL
        }
        data = add.k2data(data = data,
                          values = values)
        return(get.sapFlowDensity(method = "HFD",
                                  data = data,
                                  sapWoodDepth = sapWoodDepth(),
                                  ui.input = input))
        }
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
       if (all(is.na(values$kvalues$k))){
          return(tab.with.message(message.no.k))
          } else if (get.sapFlowSum() == 0){ 
            tab.with.message(message.no.sapflow)
             } else {
                get.treeWaterUseByMethod(data = sapFlow(),
                                         ui.input = input)
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
               !is.null(input$sapFlowMetric0flow) |
               input$sf_style != "sf_grouped" | 
               input$sf_formula == "Positve" | 
               input$sf_grouped_go |
               input$sf_negative_go |
               input$setData |
               input$FilterApply |
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
         if (all(is.na(values$kvalues$k))){
            plot.emptyMessage(message = message.no.k)
         } else {
          plot.sf.helper(data = sapFlowDens(),
                      ui.input = input)
        }
      }
    })
    
    #' Eventlistener to show figure of sap flow density
    #' (Sap Flow > Sap Flow Metrics > Figure > Diurnal pattern)
    output$sapFlowMetric <- renderPlot({
      sapFlowMetricPlot()
    })
    
    #' Eventlistener to save sap flow density plot
    #' (Sap Flow > Sap Flow Metrics > Figures > Diurnal pattern)
    observeEvent(input$save.sapFlowMetricPlot, {
      save.figure(path = projectPath(),
                  name = paste(input$sf_y_axis, input$sf_formula, sep = "_"), 
                  plotObject = sapFlowMetricPlot(), 
                  ui.input = input)
    })
    
    #' Eventlistener to save sap flow density plot
    #' vertical profile
    #' (Sap Flow > Sap Flow Metrics > Figures > Radial profile)
    observeEvent(input$save.sapFlowMetrics, {
      if (is.null(values$kvalues)){
        d = deltaTempLong()
      } else {
        d = sapFlowDens()
      }
      save.csv(path = projectPath(), 
               name = paste(input$sf_y_axis, input$sf_formula, sep = "_"),
               csvObject = d, 
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
         if (all(is.na(values$kvalues$k))){
            plot.emptyMessage(message = message.no.k)
            } else {
            plot.sf.helper(data = sapFlowDens(),
                           ui.input = input,
                           radial.profile = TRUE)
            }
      }
    })
    
    #' Eventlistener to show figure of sap flow density
    #' vertical profile
    #' (Sap Flow > Sap Flow Metrics > Figures > Radial profile)
    output$sapFlowMetric.RadialProfile <- renderPlot({
      sapFlowMetricPlot.RadialProfile()
    })
    
    #' Eventlistener to save sap flow density plot
    #' (Sap Flow > Sap Flow Metrics > Figures > Radial profile)
    observeEvent(input$save.sapFlowMetric.RadialProfile, {
      save.figure(path = projectPath(),
                  name = paste(input$sf_y_axis, input$sf_formula, 
                               "r-profil", sep = "_"),
                  plotObject = sapFlowMetricPlot.RadialProfile(), 
                  ui.input = input)
      
    })

    ##### Sap Flow Metrics Negative Formula Control Plot #####
    
    #' Reactive variable holding figure of sap flow metrics
    #' radial profile, represented as boxplots
    sapFlowMetricPlot.NegControl = eventReactive(triggerSfPlotUpdate(), {
      if (input$sf_formula == "Negative" & input$sf_y_axis == "SFS"){
        plot.sf.neg.control(data = sapFlowDens(),
                            ui.input = input)
      } else {
         if (all(is.na(values$kvalues$k))){
            plot.emptyMessage(message = message.no.k)
            } else {
               plot.emptyMessage(message = 
                               "This figure is only available for SFS \n(not SFI or SFD) when negative \nSFS-formula is applied.")
        }
      }
    })
    
    #' Eventlistener to show figure of sap flow density
    #' vertical profile
    #' (Sap Flow > Sap Flow Metrics > Figures > Negative...)
    output$sapFlowMetric.NegControl <- renderPlot({
      sapFlowMetricPlot.NegControl()
    })
    
    #' Eventlistener to save sap flow density plot
    #' (Sap Flow > Sap Flow Metrics > Figures > Negative...)
    observeEvent(input$save.sapFlowMetric.NegControl, {
      save.figure(path = projectPath(),
                  name = "SFS_Negative_control",
                  plotObject = sapFlowMetricPlot.NegControl(), 
                  ui.input = input)
      
    })
    
    
    ##### Sap Flow Rate #####
    ###### Diurnal Pattern ######
    
    get.sapFlowSum = reactive({
       if (click() > 0){
          groups = get.selectedMethods(input)
          sapFlow = sapFlow()
          return(sum(sapFlow[, groups], na.rm = T))
       } else {
          return(0)
       }
       
    })
    
    #' Reactive variable holding figure of sap flow rate
    #' for selected methods
    sapFlowTreePlot <- reactive({
       if (all(is.na(values$kvalues$k))){
          plot.emptyMessage(message = message.no.k)
          } else {
             if (get.sapFlowSum() == 0){ 
                     plot.emptyMessage(message = message.no.sapflow)
                } else {
                   plot.sapFlowRate(data = sapFlow(), 
                                    ui.input = input)
                }
          }
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
       if (all(is.na(values$kvalues$k))){
          plot.emptyMessage(message = message.no.k)
          } else {
             if (get.sapFlowSum() == 0){ 
                plot.emptyMessage(message = message.no.sapflow)
             } else {
                plot.sapFlowDay(data = sapFlow(),
                                ui.input = input)
             } 
      }
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
       if (all(is.na(values$kvalues$k))){
          plot.emptyMessage(message = message.no.k)
          } else {
             if (get.sapFlowSum() == 0){ 
                plot.emptyMessage(message = message.no.sapflow)
             } else {
                plot.twu.radialprofile(data = sapFlow(), 
                                       ui.input = input)
             } 
      }
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
    output$TWUtable <- DT::renderDataTable(rownames = FALSE, {
       treeWaterUse()
    }, options = list(scrollX = TRUE, searching = F)) #dom = 't')
    
    
    #' Eventlistener to save daily tree water use as csv
    #' (Sap Flow > Sap Flow > Tree water use)
    observeEvent(input$save.TWUCsv, {
      save.csv(path = projectPath(), 
               name = "TWU",
               csvObject = treeWaterUse(), 
               ui.input = input)
    })
})
