
shinyServer(function(input, output, session) {
  
    ###############
    ### PROJECT ###
    ###############
    
    #### Variables ####
    folderInput1 <- shinyDirChoose(input, 'folder',
                   roots=c(wd='.'), filetypes=c('', 'txt'))
    
    projectPath <- reactive({
        parseDirPath(c(wd='.'), input$folder)
    })

    # projectName <- reactive({
    #     return(tail(unlist(strsplit(as.character(projectPath()), "/")), 1))
    # })
    
    projectName <- reactive({
      return(tail(unlist(strsplit(as.character(projectPath()), "/")), 1))
    })
    
    figTitle <- reactive({
        if (input$prjNameAsTitle){
            return(projectName())
        } else {
            return(input$figTitle)
        }
    })
    
    #### Text output ####
    
    output$prjDir <- renderPrint({
        print(projectPath())
    })
    
    #### Buttons ####
    
    observeEvent(input$crtPrj, {
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
    
    values <- reactiveValues(deltaTempLong = NULL)
    
    rawData <- reactive({
      if (is.null(input$file1)){
        defaultData = "./tests/ICT_rawdata.csv"
        print("Default data")
        return(get.temperatures.ICT(defaultData,
                                    header = T, sep = ",",
                                    skip = 10))
      } else {
        return(get.rawData(input$file1, input$inputType,
                           header = input$header, sep = input$sep,
                           skip = input$skip))
      }
        
    })
    
    deltaTempLongNoFilter <- reactive({
      if (input$inputType == "ICT_raw"){
        d = get.delta.from.temp(rawData(), positions())
      }
      if (input$inputType == "ICT_delta"){
        d = get.delta.temp(rawData(), positions())
      }
      return(d)
      
    })
  
    deltaTempLong <- reactive({
      if (is.null(values$deltaTempLong)){
        values$deltaTempLong <- deltaTempLongNoFilter()
        
      }
      
      return(values$deltaTempLong)
    })
    

    deltaTempLong.depth <- reactive({
      deltaTempLong() %>% 
        filter(position == input$kPositionSelect)
    })
    
    
    positions <- reactive({
      # req(input$setData)
      if (!is.null(input$file1)){  # ToDO replace with req(input$file1)
        req(input$setData)
      } 
      
      positions = get.positions(positionManual = input$positionManual,
                          inputType = input$inputType,
                          dataSource = rawData(),
                          positionInput = input$positionInput)
      return(positions)
    })
    
    
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
    
    #### FILTER ####
    
    filterData <- function(d){

      d$doy <- as.numeric(d$doy)
      
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
      
      
      if (input$removeOutlier){
        d = remove.outlier(d, input$filterPlot_X)
      }
      
      if (input$removeNA){
        d = d[complete.cases(d), ]
      }
      
      
      print(nrow(d))
      return(d)
    }

    
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
          dateRangeInput("daterange", "Date range:"),
          # actButton("updateDate", "Update data", "update"),
          fluidRow(
            column(6, numericInput("timerangeStart", "Start", value = 0)),
            column(6, numericInput("timerangeEnd", "End", value = 24)),
          ),
          checkboxInput("removeOutlier", "Remove outlier", F),
          
          checkboxInput("removeNA", "Remove NA-rows", F),

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
        return(rawData())
    }, options = list(scrollX = TRUE))
    
    output$raw.long <- DT::renderDataTable({ # raw data
        return(deltaTempLong())
    }, options = list(scrollX = TRUE))
    
    
    #### Text output ####
    
    output$positions <- renderPrint({
        # cat("As atomic vector:\n")
        print(positions())
    })
    
    output$dataPoints <- renderText({
      paste(nrow(deltaTempLong()), " data points remaing.")
    })
    
    #### Graphics ####

    filterPlot <- reactive({
      plot.histogram(data = deltaTempLong(), 
                     x.col = input$filterPlot_X, 
                     fill.col = input$filterPlot_col, 
                     binwidth = input$filterPlot_binwidth,
                     type = input$filterPlot_type,
                     facetGrid = input$filterPlot_facetGrid,
                     scales = input$filterPlot_scales)
      
    })
    
    
    deltaTfacetWrap <- reactive({
        plot.deltaTfacetWrap(data = deltaTempLong(), 
                             xRange = input$rawPlot.x, 
                             yRange = input$rawPlot.y, 
                             scales = input$rawPlot_scales, 
                             facetWrap = input$rawPlot_facetWrap,
                             facet = input$rawPlot.facet)
    })
    
    deltaTSingle <- reactive({
        plot.singleTemperature(data = deltaTempLong(),
                               x.col = input$rawPlot.xcol, 
                               y.col = input$rawPlot.ycol, 
                               col.col = input$rawPlot.col,  
                               shape.col = input$rawPlot.shape, 
                               facetWrap = input$rawPlot_facetWrap,
                               xRange = input$rawPlot.x,
                               yRange = input$rawPlot.y, 
                               scales = input$rawPlot_scales,
                               facet = input$rawPlot.facet)
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
                     "temperatureDifferences_longFormat", sep = "")
        save.csv(path, csvObject)
        
    })
    
    observeEvent(input$save.deltaTfacetWrap, {
        name = paste(projectPath(),
                     "/graphics/",
                     "temperatureDifferences_", input$rawPlot.facet, sep = "")
        obj = deltaTfacetWrap()
        save.figure(name, obj, figTitle(), input$figFor)
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
        save.figure(name, obj, figTitle(), input$figFor)
    })

    
    ####################
    ### K-ESTIMATION ###
    ####################
    
    #### UI #####
    
    output$kPositionSelect <- renderUI({
      radioButtons("kPositionSelect", "Sensor position",
                   choices = positions(),
                   selected = 1, inline = T)
    })
    
    #### Variables ####
    
    cleanedDataAndKvalues <- reactive({ # get cleaned data for regression plot
        d = deltaTempLong() %>%
            filter(position == input$kPositionSelect)
        return(clean.data.iteration(d, 0))
    })
    
    
    kValue <- reactive({     # get k value by selected method for selected position
        if (input$kMethod == "manual"){
            kManual = input$kManual
        }
        if (input$kMethod == "csv"){
            d = kFromCsv()
            kManual = d[d$position == input$kPositionSelect, "k"]
        }
        get.kByMethod(method = input$kMethod, 
                      data = deltaTempLong(),
                      position = input$kPositionSelect,
                      kManual = kManual)
    })

    kComplete <- reactive({  # get k-values for all positions for closest, regression
        get.kByMethodAll(deltaTempLong())
    })
    
    
    kFromCsv <- reactive({
        kcsv = get.csvKvalues(input$file2, 
                              header = input$header2, 
                              sep = input$sep2,
                              skip = input$skip2)
        return(kcsv)
        
    })
    
    #### Store and display selected k-values

    values <- reactiveValues(df_data = NULL)  # create reactive value to store selected k-values
    
    observeEvent(input$setK, {  # store selected k-value in data.frame
      if (input$setK[1] == 1){
        values$df_data <-  data.frame(position = positions(),  
                                     method = rep(NA),
                                     k = rep(NA))
        values$df_data[values$df_data$position == input$kPositionSelect, 2:3] <- cbind(method = as.character(input$kMethod),
                                                                                 k = round(kValue(), 3))
        
        
      } else {
        values$df_data[values$df_data$position == input$kPositionSelect, 2:3] <- cbind(method = as.character(input$kMethod),
                                                                                 k = round(kValue(), 3))
      }
        
    })
    

   

    #### Text outputs ####

    output$kCurrent <- renderPrint({  # output current k-value (depends on selected position and method)
        paste("K-value", round(kValue(), 3))
    })
    
    
    #### Table outputs ####
    
    output$kSelected <- DT::renderDataTable({  # display selected k-values
        return(values$df_data)
    }, options = list(scrollX = TRUE))
    
    
    output$kRegression <- DT::renderDataTable({  # display estimated k-values - by regression
        return(kComplete()$regression  %>% round(., 2))
    }, options = list(scrollX = TRUE))
    
    
    output$kClosest <- DT::renderDataTable({  # output closest k-estimates
        return(kComplete()$closest %>% round(., 2))
    }, options = list(scrollX = TRUE))


    output$uploadedKvalues <- DT::renderDataTable({  # output closest k-estimates
        return(kFromCsv())
    }, options = list(scrollX = TRUE))
    
    
    
    #### Graphics ####
    kplot1 <- reactive({
        plot.kEst1(deltaTempLong.depth(),
                   cleanedDataAndKvalues()[[1]],
                   input$k1Plot.x,
                   input$k1Plot.fullrange,
                   input$k1Plot_scales)
    })
    
    kplot2 <- reactive({
        plot.kEst2(deltaTempLong.depth(),
                   cleanedDataAndKvalues()[[1]],
                   kValue(),
                   input$k1Plot.x,
                   input$k1Plot.fullrange,
                   input$k1Plot_scales)
    })
    
    kplot3 <- reactive({
        plot.kEst3(deltaTempLong.depth(), 
                   cleanedDataAndKvalues()[[1]],
                   kValue(),
                   input$k1Plot.x,
                   input$k1Plot_scales)
    })
    
    output$kvaluePlot1 <- renderPlot({ kplot1() })
    output$kvaluePlot2 <- renderPlot({ kplot2() })
    output$kvaluePlot3 <- renderPlot({ kplot3() })
    
    #### Buttons ####
    # save data
    
    observeEvent(input$save.kValues, { # save selected k-values as csv
        path = paste(projectPath(), 
                     "/csv-files", sep = "")
        csvObject = values$df_data
        save.csv(paste(path, "/k-values", sep = ""), csvObject)
    })
    
    # save figures
    observeEvent(input$save.kPlots, {
        name = paste(projectPath(),
                     "/graphics/",
                     sep = "")
        save.figure(paste(name, "k_fig1_position_", input$kPositionSelect, sep = ""), 
                    kplot1(), figTitle(), input$figFor)
        save.figure(paste(name, "k_fig2_position_", input$kPositionSelect, sep = ""), 
                    kplot2(), figTitle(), input$figFor)
        save.figure(paste(name, "k_fig3_position_", input$kPositionSelect, sep = ""), 
                    kplot3(), figTitle(), input$figFor)
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
          swd = input$stemCircumference / (2*pi) - input$barkThickness - input$heartWoodDepth
        }} else {
        swd = input$sapWoodDepth
      }
      return(swd)
    })
    
    
    sapFlowDens <- reactive({
      req(input$setK)
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
                                Dst = input$ThermalDiffusivity,
                                Zax = input$Zax, 
                                Ztg = input$Ztg)
      return(data)
      
    })
    
    sapFlow <- reactive({
      methods <- list("treeScaleSimple1" = input$treeScaleSimple1,
                   "treeScaleSimple2" = input$treeScaleSimple2,
                   "treeScaleSimple3" = input$treeScaleSimple3)
      print("METHODS")
      print(methods)
      
      df = data.frame()
      for (m in 1:length(methods)){
        print(m)
        if (methods[[m]]){
          method = names(methods)[m]
          print(method)
          d = get.sapFlowByMethod(data = deltaTempLong(),
                                  method = method, 
                                  swd = sapWoodDepth(), 
                                  sensor.dist = 10) %>% 
            mutate(sfMethod = method)
          print("HEAD D")
          
          print(head(d))
          df = bind_rows(df, d)
          
        } 
      }        
      print("HEAD DF")
      print(head(df))
      return(df)
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sfIndex, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexComplete", sep = "")
        obj = sapFlowIndex()
        save.figure(name, obj, figTitle(), input$figFor)
    })
    
    observeEvent(input$save.sfIndex.day, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexDaily", sep = "")
        obj = sapFlowIndex.Day()
        save.figure(name, obj, figTitle(), input$figFor)
    })
    
    
    #### Graphics ####
    sapFlowIndex <- reactive({
      plot.sapFlowIndex(deltaTempLong(), 
                        input$sfIndexPlot.y, 
                        input$sfIndexPlot_scales,
                        input$sfIndexPlot.wrap)
    })
    
    sapFlowIndex.Day <- reactive({
      plot.sapFlowIndex.Day(deltaTempLong(), 
                            input$sfIndexPlot.x, 
                            input$sfIndexPlot.y, 
                            input$sfIndexPlot_scales,
                            input$sfIndexPlot.wrap)
    })
    
    
    sapFlowTreePlot <- reactive({
      sapFlow() %>% 
        ggplot(.)+
        geom_line(aes(x = datetime, y = sf*100, color = sfMethod)) +
        theme_bw()
    })
    
    
    output$sapFlowIndex <- renderPlot({
      sapFlowIndex()
    })
    
    output$sapFlowIndex.Day <- renderPlot({
      sapFlowIndex.Day()
    })
    
    sapFlowDensityPlot = reactive({
      print(input$setK)
      if (input$setK[1] == 0){
        ggplot() +
          annotate(geom="text", x=5, y=5, 
                   label="No k-values have been set yet.",
                   color="red", size = 8) +
          theme_void()
      } else {
        plot.sapFlowDensity(data = sapFlowDens(), 
                            y = input$sapFlowDensityPlot.y,
                            col = input$sapFlowDensityPlot.color, 
                            scales = input$sapFlowDensityPlot_scales, 
                            facetWrap = input$sapFlowDensityPlot_facetWrap, 
                            facet.col = input$sapFlowDensityPlot.facet)
      }
      
    })
    
    output$sapFlowDensity <- renderPlot({
      sapFlowDensityPlot()
    })
    
    output$SapFlowPlot <- renderPlot({
      print(length(sapFlow()))
      if (length(sapFlow()) == 0){
        ggplot() +
          annotate(geom="text", x=5, y=5, 
                   label="No k-values have been set yet or no method have been selected.",
                   color="red", size = 8) +
          theme_void()
      } else {
        sapFlowTreePlot()
      }
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sapFlowDensityPlot, {
      name = paste(projectPath(),
                   "/graphics/",
                   "sf_", input$sapFlowDensityPlot.y, sep = "")
      obj = sapFlowDensityPlot()
      save.figure(name, obj, figTitle(), input$figFor)
    })
    
    observeEvent(input$save.sapFlowDensity, {
      name = paste(projectPath(),
                   "/csv-files/",
                   "sapFlowDensity", sep = "")
      obj = sapFlowDens()
      save.csv(name, obj)
    })
    
})
