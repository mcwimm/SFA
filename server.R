
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
        
        # if (!is.null(input$daterange)){
        #     req(input$updateTime)
        #     print("Inside if daterange")
        #     print(input$daterange[1])
        #     d = d %>%
        #         filter(Date >= input$daterange[1]) %>%
        #         filter(Date <= input$daterange[2])
        # }
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
    
    ### Sap flow
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
