
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
    
    rawData <- reactive({
        print(input$inputType)
        return(get.rawData(input$file1, input$inputType,
                          header = input$header, sep = input$sep,
                          skip = input$skip))
    })
    
    deltaTempLong <- reactive({
        if (input$inputType == "ICT_raw"){
            d = get.delta.from.temp(rawData(), depths())
        }
        if (input$inputType == "ICT_delta"){
            d = get.delta.temp(rawData(), depths())
        }
        # print(as.Date(input$daterange[1]) - 1)
        # print(as.Date(input$daterange[2]) + 1)
        # 
        # print(paste("Nrow before:  ", nrow(d)))
        # 
        # d = d[which(as.POSIXct(d$datetime) > as.Date(input$daterange[1]) - 1  &
        #                 as.POSIXct(d$datetime) < as.Date(input$daterange[2]) + 1), ]
        # print(paste("Nrow after:  ", nrow(d)))
        # 
        # print(paste("Nrow before:  ", nrow(d)))
        
        
        return(d)
    })
    
    deltaTempLong.depth <- reactive({
        deltaTempLong() %>% 
            filter(depth == input$kDepthSelect)
    })
    
    depths <- reactive({
        return(get.depths(depthManual = input$depthManual,
                          inputType = input$inputType,
                          dataSource = rawData(),
                          depthInput = input$depthInput))
    })
    
    ### data file insights for ui ###

    minMaxDatetime <- reactive({
        # d = deltaTempLong()
        d = rawData()
        minDate = as.Date(d[which.min(as.POSIXct(d$datetime)),
                            "datetime"])
        maxDate = as.Date(d[which.max(as.POSIXct(d$datetime)),
                            "datetime"])
        print(c(minDate, maxDate))
        return(c(minDate, maxDate))
    })
    
    observe({
        req(input$getTimeRange)

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
    
    output$depths <- renderPrint({
        cat("As atomic vector:\n")
        print(depths())
    })
    
    
    #### Graphics ####

    
    deltaTfacetWrap <- reactive({
        plot.deltaTfacetWrap(data = deltaTempLong(), 
                             xRange = input$rawPlot.x, 
                             yRange = input$rawPlot.y, 
                             scales = input$rawPlot.scales, 
                             facetWrap = input$rawPlot.facetWrap,
                             facet = input$rawPlot.facet)
    })
    
    deltaTSingle <- reactive({
        plot.singleTemperature(data = deltaTempLong(),
                               x.col = input$rawPlot.xcol, 
                               y.col = input$rawPlot.ycol, 
                               col.col = input$rawPlot.col,  
                               shape.col = input$rawPlot.shape, 
                               facetWrap = input$rawPlot.facetWrap,
                               xRange = input$rawPlot.x,
                               yRange = input$rawPlot.y, 
                               scales = input$rawPlot.scales,
                               facet = input$rawPlot.facet)
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
    
    #### Variables ####
    
    cleanedDataAndKvalues <- reactive({ # get cleaned data for regression plot
        d = deltaTempLong() %>%
            filter(depth == input$kDepthSelect)
        return(clean.data.iteration(d, 0))
    })
    
    
    kValue <- reactive({     # get k value by selected method for selected depth
        if (input$kMethod == "manual"){
            kManual = input$kManual
        }
        if (input$kMethod == "csv"){
            d = kFromCsv()
            kManual = d[d$depth == input$kDepthSelect, "k"]
        }
        get.kByMethod(method = input$kMethod, 
                      data = deltaTempLong(),
                      depth = input$kDepthSelect,
                      kManual = kManual)
    })

    kComplete <- reactive({  # get k-values for all depths for closest, regression
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
    
    observeEvent(input$kCreate, {  # create empty dataframe with depths to store selected k-values
        values$df_data <-  data.frame(depth = depths(),  
                                      method = rep(NA),
                                      k = rep(NA))
    })
    
    observeEvent(input$setK, {  # store selected k-value in data.frame
        values$df_data[values$df_data$depth == input$kDepthSelect, 2:3] <- cbind(method = as.character(input$kMethod),
                                                                                 k = round(kValue(), 3))
    })
    

   

    #### Text outputs ####

    output$kCurrent <- renderPrint({  # output current k-value (depends on selected depth and method)
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
                   kValue())
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
        save.figure(paste(name, "k_fig1_depth_", input$kDepthSelect, sep = ""), 
                    kplot1(), figTitle(), input$figFor)
        save.figure(paste(name, "k_fig2_depth_", input$kDepthSelect, sep = ""), 
                    kplot2(), figTitle(), input$figFor)
        save.figure(paste(name, "k_fig3_depth_", input$kDepthSelect, sep = ""), 
                    kplot3(), figTitle(), input$figFor)
    })

    #########################
    ##### SAP FLOW INDEX ####
    #########################
    
    
    
    #### Variables ####
    sapFlow <- reactive({
      req(input$setK)
      kValues = values$df_data
      kValues[, "k"] = as.numeric(kValues[, "k"])
      depths = unique(kValues[!is.na(kValues$k), ]$depth)
      data = deltaTempLong()
      data = data[data$depth %in% depths, ]
      data = merge(data, kValues[, c("depth", "k")], by = "depth")
      data = data[!is.na(data$datetime), ]
      
      data = get.sapFlowDensity(method = "HFD",
                                data = data,
                                sapWoodDepth = input$sapWoodDepth,
                                Dst = input$ThermalDiffusivity,
                                Zax = input$Zax, 
                                Ztg = input$Ztg)
      return(data)
      
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sfIndex, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexComplete", sep = "")
        obj = plot.sapFlowIndex(deltaTempLong(), 
                                input$sfIndexPlot.y, 
                                input$sfIndexPlot.scales,
                                input$sfIndexPlot.wrap)
        save.figure(name, obj, figTitle(), input$figFor)
    })
    
    observeEvent(input$save.sfIndex.day, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexDaily", sep = "")
        obj = plot.sapFlowIndex.Day(deltaTempLong(), 
                                    input$sfIndexPlot.x, 
                                    input$sfIndexPlot.y, 
                                    input$sfIndexPlot.scales)
        save.figure(name, obj, figTitle(), input$figFor)
    })
    
    
    #### Graphics ####
    
    output$sapFlowIndex <- renderPlot({
        plot.sapFlowIndex(deltaTempLong(), 
                          input$sfIndexPlot.y, 
                          input$sfIndexPlot.scales,
                          input$sfIndexPlot.wrap)
    })
    
    output$sapFlowIndex.Day <- renderPlot({
        plot.sapFlowIndex.Day(deltaTempLong(), 
                              input$sfIndexPlot.x, 
                              input$sfIndexPlot.y, 
                              input$sfIndexPlot.scales)
    })
    
    sapFlowDensityPlot = reactive({
      plot.sapFlowDensity(data = sapFlow(), 
                          y = input$sapFlowDensityPlot.y,
                          col = input$sapFlowDensityPlot.color, 
                          scales = input$sapFlowDensityPlot.scales, 
                          facetWrap = input$sapFlowDensityPlot_facetWrap, 
                          facet.col = input$sapFlowDensityPlot.facet)
    })
    
    output$sapFlowDensity <- renderPlot({
      sapFlowDensityPlot()
    })
    
    #### Buttons ####
    
    observeEvent(input$save.sapFlowDensityPlot, {
      name = paste(projectPath(),
                   "/graphics/",
                   "sapFlowDensityPlot", sep = "")
      obj = sapFlowDensityPlot()
      save.figure(name, obj, figTitle(), input$figFor)
    })
    
    observeEvent(input$save.sapFlowDensity, {
      name = paste(projectPath(),
                   "/csv-files/",
                   "sapFlowDensity", sep = "")
      obj = sapFlow()
      save.csv(name, obj)
    })
    
})
