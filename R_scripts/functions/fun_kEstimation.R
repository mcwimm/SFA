
get.kByMethod <- function(data, ui.input){
   # Extract ui-input values
   method = ui.input$kMethod
   sensorDepth = ui.input$kPositionSelect
   nightTimeStart = ui.input$kRegressionTime.start
   nightTimeEnd = ui.input$kRegressionTime.end
   
   if (method == "manual"){
      k = ui.input$kManual
   }
   
   if (method == "regression"){
      k = get.regressionK.depth(data, sensorDepth, ui.input)[, "k"]
   }
   
   if (method == "no.flow"){
      allK = get.zeroflowKvalues(data)
      k = allK[allK$position == sensorDepth, "k"][[1]]
   }
   
   if (method == "csv"){
      req(ui.input$file2)
      kcsv = get.csvKvalues(ui.input = ui.input)
      k = kvalues[kvalues$position == ui.input$kPositionSelect, "k"]
   }
   return(k)
}

get.kByMethodAll <- function(data, ui.input){
   return(list( "regression" = get.regressionKvalues(data, ui.input),
                "no.flow" = get.zeroflowKvalues(data)))
}

fill.k.table = function(method, k.data, ui.input, reactive.value){
   # Define method name
   if (method == "regression"){
      method_name = "auto. regression"
      if (ui.input$dTimeFilter){
         method_name = paste(method_name, " (",
                             ui.input$kRegressionTime.start, " - ",
                             ui.input$kRegressionTime.end, ")",
                             sep = "")
      }
   }
   if (method == "no.flow"){
      method_name = "No-flow"
   }
   
   # Fill method and k-value by row/ position
   for (pos in unique(k.data[, "position"])){
      if (method == "csv"){
         method_name = ifelse(is.null(as.character(k.data$method)), "csv",
                              paste("csv: ",
                                    as.character(k.data[k.data$position == pos, "method"]),
                                    sep = ""))
      }
      
      reactive.value$kvalues[reactive.value$kvalues$position == pos, 2:3] <- cbind(
         method = method_name,
         k = k.data[k.data$position == pos, "k"])
   }
   return(reactive.value)
}

##################
### regression ###
##################

get.regressionK.depth <- function(data, sensorDepth, ui.input){
   # Filter repective sensor depth
   data = data %>% 
      filter(position == sensorDepth) 
   datapoints_sensordepth = nrow(data)

   # Filter 0-trend data points by time, if filter is enables
   data = get.time.filtered.data(data = data,
                                 ui.input = ui.input)

   # Iterate through data
   data.adj = clean.data.iteration(data)
   
   datapoints_used = nrow(data.adj[[1]])
   print(paste(round(datapoints_used/datapoints_sensordepth*100, 1),
               " % (", datapoints_used, "/ ", datapoints_sensordepth,
               " ) of data points are used to estimate k.",
               sep = ""))
   
   # Combine results into a table
   df = data.frame(kAs = data.adj[[2]][[1]],
                   R.kAs = data.adj[[2]][[2]],
                   kSa = data.adj[[3]][[1]],
                   R.kSa = data.adj[[3]][[2]])
   df$k = mean(c(abs(df$kAs), abs(df$kSa)))
   return(df)
}

get.regressionKvalues <- function(data, ui.input){
   positions = unique(data$position)
   h = do.call(rbind, lapply(positions, 
                                function(x) 
                                   get.regressionK.depth(data, x, ui.input)))
   h = cbind(position = positions, h)
   return(h)
}

#### data cleaing

get.time.filtered.data = function(data, ui.input){
   if (ui.input$dTimeFilter){
      if (nightTimeStart < nightTimeEnd){
         data = data %>% 
            filter(dTime >= ui.input$kRegressionTime.start & 
                      dTime <= ui.input$kRegressionTime.end) 
      } else {
         data = data %>% 
            filter(dTime >= ui.input$kRegressionTime.start |
                      dTime <= ui.input$kRegressionTime.end) 
      }
   }
   return(data)
}

remove.right <- function(data, x.col, y.col){
   y.max <- max(data[, y.col])
   x.cutoff <- data[data[, y.col] == y.max, x.col]
   
   data <- subset(data, data[, x.col] < x.cutoff)
   return(data)
}


remove.below <- function(data,  x.col, y.col){
   x.min <- min(data[, x.col])
   y.cutoff <- data[data[, x.col] == x.min, y.col]
   
   data <- subset(data, data[, y.col] > y.cutoff)
   return(data)
}

cleaning <- function(data, x.col, y.col){
   # data <- remove.outlier(data, data[, x.col])
   # data <- remove.outlier(data, data[, y.col])
   
   data <- remove.below(data, x.col, y.col)
   data <- remove.right(data, x.col, y.col)
   
   
   return(data)
}

get.K.dTas <- function(clean.data){
   dTas.lm <- summary(lm(dTas ~ dTsym.dTas, 
                         clean.data))
   dTas.K <- dTas.lm$coefficients[1, 1]
   return(c(dTas.K, dTas.lm$adj.r.squared))
}

get.K.dTsa <- function(clean.data){
   dTsa.lm <- summary(lm(dTsa ~ dTsym.dTas, 
                         clean.data))
   dTsa.K <- dTsa.lm$coefficients[1,1]
   return(c(dTsa.K, dTsa.lm$adj.r.squared))
}

clean.data.iteration = function(data_ini, initial.cutoff = 0){
   data <- data_ini[data_ini$timestep > initial.cutoff, ]
   step = 0
   r.adj.n1 = 0
   r.adj.diff = 1
   data.n1 = data 
   
   while (r.adj.diff > 0.01){
      # print(paste("Step ", step))
      data = cleaning(data, x.col = "dTsym.dTas", y.col = "dTas")
      # print(paste("nrow(data)  ", nrow(data)))
      
      if (nrow(data) < 50){
         print(paste("[Warning] Iteration stopped as number of data points < 50."))
         data = data.n1
         break
      }
      
      K.dTas <- get.K.dTas(clean.data = data)
      r.adj.n <- K.dTas[2]   
      
      r.adj.diff <- r.adj.n - r.adj.n1
      
      # print(paste("r.adj.n  ", r.adj.n))   
      # print(paste("r.adj.n1  ", r.adj.n1))
      # print(paste("r.adj.diff  ", r.adj.diff))
      
      if (r.adj.diff < 0){
         data = data.n1 
      }
      
      r.adj.n1 <- r.adj.n  
      data.n1 <- data
      step = step + 1
      
   }

   K.dTas <- get.K.dTas(clean.data = data)
   K.dTsa <- get.K.dTsa(clean.data = data)

   return(list(data, K.dTas, K.dTsa))
}



#################
### zero-flow ###
#################

get_min_at_min <- function(vec1, vec2) {
   return(vec2[order(vec1, vec2)[1]])
}


get.zeroflowKvalues <- function(data){
   zeroFlowK = data %>% 
      group_by(position) %>% 
      mutate(abs = abs((dTsym.dTas))) %>% 
      mutate(min_distance = min(abs, na.rm = T)) %>% 
      filter(abs == min_distance) %>% 
      select(position, abs, min_distance, dTas) %>% 
      distinct(position, min_distance, k = median(dTas)) %>% 
      rename("distance y-axis" = min_distance)
   return(zeroFlowK)
}



#################
### from file ###
#################

get.csvKvalues <- function(ui.input){
   file = ui.input$file2
   
   kValues <- read.csv(file$datapath,
                       header = ui.input$header2, 
                       sep = ui.input$sep2, 
                       fileEncoding="latin1",
                       skip = ui.input$skip2)
   return(kValues)
}



##############
### manual ###
##############

