shinyServer(function(input, output) {

    prjFolderList <- reactive({
        req(input$UpdatePrjFolder)
        fileList = list.files("./output/", full.names = T)
        return(
            cbind(
                file.info(fileList)[,c("size"), drop=FALSE],
                x = as.character(file.mtime(fileList))) %>% 
                separate(x,
                         into = c("DateModified","TimeModified"),
                         sep = " ") %>% 
                rownames_to_column() %>% 
                select(FileName=rowname,
                       Size=size,
                       DateModified,
                       TimeModified) %>% 
                separate(FileName, c("dir", "folder", "prjName"),
                         sep = "/") %>% 
                select(-folder, -dir) 
        )
    })
    
    fileName <- reactive({
        fn = unlist(strsplit(as.character(input$file1), "\\."))[1]
        return(fn)
    })
    
    projectName <- reactive({
        req(input$setProject)
        if (input$prjNew){
            return(paste(input$prjName, Sys.Date(), sep = "_"))
        } else {
            print(input$prjName)
            return(input$prjName)
        }
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
    
    output$prjNameOut <- renderPrint({
        print(projectName())
    })

    #################
    #### Buttons ####
    #################
    
    ### Download ###
    
    # output$DownloadProject <- downloadHandler(
    #     filename = function() {
    #         # paste("input$dataset", ".csv", sep = ".")
    #         paste("output", "zip", sep=".")
    #     },
    #     
    #     # This function should write data to a file given to it by
    #     # the argument 'file'.
    #     content = function(fname) {
    #         fs <- c()
    #         tmpdir <- tempdir()
    #         setwd(tempdir())
    #         for (i in c(1,2,3,4,5)) {
    #             path <- paste0("sample_", i, ".csv")
    #             fs <- c(fs, path)
    #             write(i*2, path)
    #         }
    #         zip(zipfile=fname, files=fs)
    #         # sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
    #         
    #         # Write to a file specified by the 'file' argument
    #         # write.table(deltaTempLong(), file)
    #         # write.csv(deltaTempLong(), file)
    #     },
    #     contentType = "application/zip"
    #     
    # )
    
    
    ### SETTINGS ###
    
    observeEvent(input$setProject, {
        if (input$prjName == ""){
            showNotification("Error: Project name is missing!",
                             type = "error")
            return(NULL)
        }
        
        if (input$prjNew){
            if (!dir.exists("./output")){
                dir.create("./output")
            }
            if (dir.exists(paste("./output/", projectName(), sep = ""))){
                prjText = "Project already exists. Please choose another project name."
            } else {
                dir.create(paste("./output/", projectName(), sep = ""))
                dir.create(paste("./output/", projectName(),
                                 "/csv-files/",
                                 sep = ""))
                dir.create(paste("./output/", projectName(),
                                 "/graphics/",
                                 sep = ""))
                
                prjText = "Project folder successfully created."
            }
            
        } else {
            if (dir.exists(paste("./output/", projectName(), sep = ""))){
                prjText = "Existing project will be used."
            } else {
            prjText = "Project does not exist."
            }
            
        }
        output$prj = renderText(prjText)
        
    })
    
    observeEvent(input$UpdatePrjFolder, {
        prjFolderList()
    })
    
    
    
    ### DATA ###
    
    observeEvent(input$save_dat_upl, {
        csvObject = deltaTempLong()
        fn = paste("./output/", projectName(),
               "/csv-files/",
               "temperatureDifferences_longFormat", sep = "")
        save.csv(fn, csvObject)

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
    
    # output$save_dat_upl <- downloadHandler(
    #     filename = function() {
    #         paste("./output/", projectName(),
    #               "/csv-files/",
    #               "temperatureDifferences.csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(rawData(), file, row.names = FALSE)
    #     }
    # )

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
