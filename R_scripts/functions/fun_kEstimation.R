
get.kByMethod <- function(method, data, depth, kManual = 1.11){
   sensorDepth = depth
   if (method == "manual"){
      k = kManual
   }
   
   if (method == "regression"){
      k = get.regressionK.depth(data, depth)[, "k"]
   }
   
   if (method == "closest"){
      allK = get.closestKvalues(data)
      k = allK[allK$depth == sensorDepth, "k"][[1]]
   }
   
   if (method == "csv"){
      k = kManual
   }
   return(k)
}

get.kByMethodAll <- function(data){
   return(list( "regression" = get.regressionKvalues(data),
                "closest" = get.closestKvalues(data)))
}

##################
### regression ###
##################

get.regressionK.depth <- function(data, depth){
   sensorDepth = depth
   data = data %>% 
      filter(depth == sensorDepth)
   data.adj = clean.data.iteration(data, 0)
   
   df = data.frame(kAs = data.adj[[2]][[1]],
                   R.kAs = data.adj[[2]][[2]],
                   kSa = data.adj[[3]][[1]],
                   R.kSa = data.adj[[3]][[2]])
   df$k = mean(c(abs(df$kAs), abs(df$kSa)))
   return(df)
}

get.regressionKvalues <- function(data){
   depths = unique(data$depth)
   return(do.call(rbind, lapply(depths, function(x) get.regressionK.depth(data, x))))
}

#### data cleaing

remove.outlier <- function(data, data.vector){
   Q <- quantile(data.vector, probs=c(.25, .75), na.rm = FALSE)
   iqr <- IQR(data.vector)
   up <-  Q[2] + 1.5 * iqr # Upper Range  
   low <- Q[1] - 1.5 * iqr # Lower Range???
   
   data <- subset(data, data.vector > (Q[1] - 1.5*iqr) & 
                     data.vector < (Q[2]+1.5*iqr))
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
   data <- remove.outlier(data, data[, x.col])
   data <- remove.outlier(data, data[, y.col])
   
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

clean.data.iteration = function(data, initial.cutoff){
   data <- data[data$timestep > initial.cutoff, ]
   
   step = 0
   r.adj.n1 = 0
   r.adj.diff = 1
   data.n1 = data 
   
   while (r.adj.diff > 0.01){
      # print(paste("Step ", step))
      data = cleaning(data, x.col = "dTsym.dTas", y.col = "dTas")
      # print(paste("nrow(data)  ", nrow(data)))
      
      if (nrow(data) < 50){
         print("Less than 50 data points remaining.")
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



###############
### closest ###
###############

get_min_at_min <- function(vec1, vec2) {
   return(vec2[order(vec1, vec2)[1]])
}

get.closestKvalues <- function(data){
   
   return(data %>% 
             group_by(depth) %>% 
             summarise("Min dTsym.dTas" = min(dTsym.dTas),
                       "k" = get_min_at_min(dTsym.dTas, dTas)))
   
}


#################
### from file ###
#################

get.csvKvalues <- function(file, header, sep, skip){

   kValues <- read.csv(file$datapath,
                       header = header, sep = sep, 
                       fileEncoding="latin1",
                       skip = skip)
   return(kValues)
}



##############
### manual ###
##############

