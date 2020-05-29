
shinyServer(function(input, output) {
    # define output directory
    folderInput1 <- shinyDirChoose(input, 'folder',
                   roots=c(wd='.'), filetypes=c('', 'txt'))
    projectName <- reactive({
        parseDirPath(c(wd='.'), input$folder)
    })

    fileName <- reactive({
        fn = unlist(strsplit(as.character(input$file1), "\\."))[1]
        return(fn)
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
            return(get.delta.from.temp(rawData(), depths()))
        }
        if (input$inputType == "ICT_delta"){
            return(get.delta.temp(rawData(), depths()))
        }
    })
    


    depths <- reactive({
        return(get.depths(depthManual = input$depthManual,
                          inputType = input$inputType,
                          dataSource = rawData(),
                          depthInput = input$depthInput))
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
    
    output$prjName <- renderPrint({
        print(projectName())
    })
    
    
    #################
    #### Buttons ####
    #################
    
    ### DATA ###
    
    observeEvent(input$save_dat_upl, {
        csvObject = deltaTempLong()
        path = paste(projectName(), "/temperatureDifferences_longFormat", sep = "")
        save.csv(path, csvObject)

    })
    
    observeEvent(input$save.deltaTfacetWrap, {
        name = paste("./output/", projectName(),
                     "/graphics/",
                     "temperatureDifferences", sep = "")
        obj = plot.deltaTfacetWrap(deltaTempLong(), 
                                   input$rawPlot.x, input$rawPlot.y,
                                   input$rawPlot.scales,
                                   fileName())
        save.figure(name, obj,
                    input$figFor)
    })
    
    observeEvent(input$save.sfIndex, {
        name = paste("./output/", projectName(),
                     "/graphics/",
                     "sapFlowIndexComplete", sep = "")
        obj = plot.sapFlowIndex(deltaTempLong(), 
                                input$sfIndexPlot.y, 
                                input$sfIndexPlot.scales,
                                fileName())
        save.figure(name, obj,
                    input$figFor)
    })
    
    observeEvent(input$save.sfIndex.day, {
        name = paste("./output/", projectName(),
                     "/graphics/",
                     "sapFlowIndexDaily", sep = "")
        obj = plot.sapFlowIndex.Day(deltaTempLong(), 
                                    input$sfIndexPlot.x, 
                                    input$sfIndexPlot.y, 
                                    input$sfIndexPlot.scales,
                                    fileName())
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
                          input$rawPlot.y, fileName())
    })
    
    output$deltaTfacetWrap <- renderPlot({
        plot.deltaTfacetWrap(deltaTempLong(), 
                             input$rawPlot.x, input$rawPlot.y, 
                             input$rawPlot.scales,
                             fileName())
    })
    
    ### SAP FLOW INDEX
    
    output$sapFlowIndex <- renderPlot({
        plot.sapFlowIndex(deltaTempLong(), 
                          input$sfIndexPlot.y, 
                          input$sfIndexPlot.scales,
                          fileName())
    })
    
    output$sapFlowIndex.Day <- renderPlot({
        plot.sapFlowIndex.Day(deltaTempLong(), 
                              input$sfIndexPlot.x, 
                              input$sfIndexPlot.y, 
                              input$sfIndexPlot.scales,
                              fileName())
    })
})
