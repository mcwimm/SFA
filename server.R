
shinyServer(function(input, output, session) {
    # define output directory
    folderInput1 <- shinyDirChoose(input, 'folder',
                   roots=c(wd='.'), filetypes=c('', 'txt'))
    
    projectPath <- reactive({
        parseDirPath(c(wd='.'), input$folder)
    })

    projectName <- reactive({
        return(tail(unlist(strsplit(as.character(projectPath()), "/")), 1))
    })
    
    
    ### data handling ###
    
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
    
    # observeEvent(input$updateTime, {
    #     d = deltaTempLong()
    #     print(paste("Nrow after:  ", nrow(d)))
    #     print(head(d))
    # })
    
    
    # minMaxDepth <- reactive({
    #     d = depths()
    #     return(list(min(d), max(d)))
    # })
    # 
    # observe({
    #    
    #     if (!is.null(input$file1)){
    #         minMaxDepth = minMaxDepth()
    #         print(minMaxDepth)
    #         updateSliderInput(session, "kDepthSelect",
    #                           value = minMaxDepth[1],
    #                           min = minMaxDepth[1],
    #                           max = minMaxDepth[2], 
    #                           step = 1)
    #     } else {
    #         updateSliderInput(session, "kDepthSelect",
    #                           value = 1,
    #                           min = 1,
    #                           max = 10, 
    #                           step = 1)
    #     }
    # })
    
    ### cleaned data
    
    cleanedDataAndKvalues <- reactive({
        d = deltaTempLong() %>% 
            filter(depth == input$kDepthSelect)
        data.adj = clean.data.iteration(d, 0)
        
        return(data.adj)
    }) 
    
    
    ############################
    ##### Table outputs ########
    ############################
    
    output$raw.wide <- DT::renderDataTable({ # raw data
        return(rawData())
    }, options = list(scrollX = TRUE))
    
    output$raw.long <- DT::renderDataTable({ # raw data
        return(deltaTempLong())
    }, options = list(scrollX = TRUE))
    
    
    output$prjFolder <- DT::renderDataTable({ # raw data
        return(prjFolderList())
    }, options = list(scrollX = TRUE))
    
    
    ### k-values ###

    
    # uploadedKs <- reactive({
    #     ks <- read.csv(input$file2,
    #                         header = T, sep = ",", 
    #                         fileEncoding="latin1")
    #     return(ks)
    # })
    # 
    # output$uploadedKvalues <- DT::renderDataTable({
    #     
    #     print(uploadedKs())
    #     return(uploadedKs())
    # }, options = list(scrollX = TRUE))
    
    
    kClosest <- reactive({
        get.closestKvalues(deltaTempLong())
    })
    
    output$kClosest <- DT::renderDataTable({ # raw data
        return(kClosest() %>% round(., 2))
    }, options = list(scrollX = TRUE))
    
    
    #####################
    #### Text output ####
    #####################
    
    output$depths <- renderPrint({
        cat("As atomic vector:\n")
        print(depths())
    })
    
    output$prjDir <- renderPrint({
        print(projectPath())
    })
    
    
    #################
    #### Buttons ####
    #################
    
    
    ### Settings ###
    
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
    
    ### DATA ###
    
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
                     "temperatureDifferences", sep = "")
        obj = plot.deltaTfacetWrap(deltaTempLong(), 
                                   input$rawPlot.x, input$rawPlot.y,
                                   input$rawPlot.scales,
                                   projectName())
        save.figure(name, obj,
                    input$figFor)
    })
    
    
    ### K estimation ###
    
    observeEvent(input$setK, {
        kAs <- cleanedDataAndKvalues()[[2]]
        kSa <- cleanedDataAndKvalues()[[3]]
        csvObject = data.frame(depth = input$kDepthSelect,
                       kAs = kAs[[1]],
                       rAs = kAs[[2]],
                       kSa = kSa[[1]],
                       rSa = kSa[[2]],
                       kAvg = (mean(c(abs(kAs[[1]]), abs(kSa[[1]]))))
                       )
        print(csvObject)
        path = paste(projectPath(), 
                     "/csv-files/",
                     "k_", input$kDepthSelect, sep = "")
        print(path)
        
        save.csv(path, csvObject)
        
    })
    
    observeEvent(input$save.kValues, {
        path = paste(projectPath(), 
                     "/csv-files", sep = "")
        csvObject = get.existingKvalues(path)
        save.csv(paste(path, "/k-values", sep = ""), csvObject)
    })
    
    observeEvent(input$update.Kvalues, {
        output$existingKvalues <- DT::renderDataTable({
            path = paste(projectPath(), 
                         "/csv-files", sep = "")
            return(get.existingKvalues(path)[, -1]  %>% 
                       round(., 2))
        }, options = list(scrollX = TRUE))
    })
    
    ### Sap flow ###
    observeEvent(input$save.sfIndex, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexComplete", sep = "")
        obj = plot.sapFlowIndex(deltaTempLong(), 
                                input$sfIndexPlot.y, 
                                input$sfIndexPlot.scales,
                                projectName())
        save.figure(name, obj,
                    input$figFor)
    })
    
    observeEvent(input$save.sfIndex.day, {
        name = paste(projectPath(),
                     "/graphics/",
                     "sapFlowIndexDaily", sep = "")
        obj = plot.sapFlowIndex.Day(deltaTempLong(), 
                                    input$sfIndexPlot.x, 
                                    input$sfIndexPlot.y, 
                                    input$sfIndexPlot.scales,
                                    projectName())
        save.figure(name, obj,
                    input$figFor)
    })
    

    ###################
    ##### GRAPHICS ####
    ###################
    
    ### DATA ###
    
    output$rawPlot <- renderPlot({
        d = deltaTempLong()
        plot.deltaTemperature(d,
                          input$rawPlot.y, projectName())
    })
    
    output$deltaTfacetWrap <- renderPlot({
        plot.deltaTfacetWrap(deltaTempLong(), 
                             input$rawPlot.x, input$rawPlot.y, 
                             input$rawPlot.scales,
                             projectName())
    })
    
    ### K-VALUE ESTIMATION
    
    output$kvaluePlot1 <- renderPlot({
        d = deltaTempLong() %>% 
            filter(depth == input$kDepthSelect)
        plot.kEst1(d, cleanedDataAndKvalues()[[1]],
                   input$k1Plot.x[1], input$k1Plot.x[2])

    })
    
    
    
    ### SAP FLOW INDEX
    
    output$sapFlowIndex <- renderPlot({
        plot.sapFlowIndex(deltaTempLong(), 
                          input$sfIndexPlot.y, 
                          input$sfIndexPlot.scales,
                          projectName())
    })
    
    output$sapFlowIndex.Day <- renderPlot({
        plot.sapFlowIndex.Day(deltaTempLong(), 
                              input$sfIndexPlot.x, 
                              input$sfIndexPlot.y, 
                              input$sfIndexPlot.scales,
                              projectName())
    })
    
    
})
