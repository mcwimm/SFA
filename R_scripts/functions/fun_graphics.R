get.labelledFacets = function(data, facet.col){
   facet = as.integer(data[, facet.col]) 
   facet.factor <- c(unique(facet))
   labs = unlist(lapply(facet.factor, function(x) paste(facet.col, ": ", x, sep = "")))
   return(factor(facet, labels = labs))
}

get.fillcolors = function(ui.input){
   # If no colors are defined use default set
   if (ui.input$fillColors == ""){
      print("Discrete color scheme: default")
      col = c("#d8b365", "#260C7D", "#5ab4ac", "#7D410C", 
              "#007D06", '#999999','#E69F00', '#56B4E9')
   } else {
      print("Discrete color scheme: customized")
      cols = ui.input$fillColors
      cols_split = strsplit(cols, ",")[[1]]
      col = c()
      for (i in 1:length(cols_split)){
         # remove white spaces
         c = gsub(" ", "", cols_split[i], fixed = TRUE)
         col = append(col, c)
      }
   }
   return(col)
}

get.gradientcolors = function(ui.input){
   # If no colors are defined use default set
   if (ui.input$gradientColors == ""){
      print("Gradient color scheme: default")
      col = c("#d8b365", "#5ab4ac")
      
   } else {
      print("Gradient color scheme: customized")
      cols = ui.input$gradientColors
      cols_split = strsplit(cols, ",")[[1]]
      col = c()
      for (i in 1:length(cols_split)){
         # remove white spaces
         c = gsub(" ", "", cols_split[i], fixed = TRUE)
         col = append(col, c)
      }
   }
   return(col)
}


######### labels working in shiny ggplot
# mit den Labels funktioniert die Anzeige, aber nicht das speichern an svg und beim pdf fehlen delta fehlt
labels <- list("dTsym.dTas" = expression(paste("dTsym \u22C5", dTas^-1)),
               "dTas" = "dTas",
               "dTsa" = "dTsa",
               "dTSym" = "dTSym",
               "dT" = "\U0394 T (\u00B0 C)",
               "T" = paste("Temperature (\u00B0", "C)", sep = ""),
               "doy" = "Day of year",
               "dTime" = "Time (h)",
               "position" = "Sensor position",
               "SFI" = "Sap Flow Index (\u00B0 C)",
               "SF" = expression(paste("Sap Flow rate (kg \u22C5 ",
                                       h^-1, ")")),
               "SFS" = expression(paste("Sap Flow per Section (g \u22C5 ",
                                        cm^-1, "\u22C5", h^-1, ")")),
               "SFDsw" = expression(paste("Sapwood-related Sap Flow Density (g \u22C5",
                                          cm^-2, "\u22C5", h^-1, ")")))


######### labels working for PDF but not in shiny
# labels <- list("dTsym.dTas" = expression(dTsym~"·"~dTas^-1),
#                "dTas" = "dTas",
#                "dTsa" = "dTsa",
#                "dTSym" = "dTSym",
#                "dT" = expression(Delta~T~("°"~C)),
#                "T" = expression(Delta~Temperature~("°"~C)),
#                "doy" = "Day of year",
#                "dTime" = "Time (h)",
#                "position" = "Sensor position",
#                "SFI" = "Sap Flow Index (°C)",
#                "SFS" = expression(Sap~Flow~Density~(g~"·"~cm^-2~"·"~s^-1)),
#                "SFDsw" = expression(Sapwood-related~Sap~Flow~Density~(g~"·"~cm^-1~"·"~s^-1)))




######### labels working for SVG
# "·" in expression doesn't work = can not evaluate the combination of expression and "string"
# labels <- list("dTsym.dTas" = expression(dTsym~~dTas^-1),
#                "dTas" = "dTas",
#                "dTsa" = "dTsa",
#                "dTSym" = "dTSym",
#                "dT" = "Δ T (°C)",
#                "T" = "Δ Temperature (°C)",
#                "doy" = "Day of year",
#                "dTime" = "Time (h)",
#                "position" = "Sensor position",
#                "SFI" = "Sap Flow Index (°C)",
#                "SFS" = expression(Sap~Flow~Density~(g~cm^-2~s^-1)),
#                "SFDsw" = expression(Sapwood-related~Sap~Flow~Density~(g~cm^-1~s^-1)))



# Function to produce summary statistics (mean and +/- sd)
# Source: http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
data_summary <- function(x) {
   m <- mean(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

######## FILTER ########

plot.histogram <- function(data, ui.input){
   x.col = ui.input$filterPlot_X
   fill.col = ui.input$filterPlot_col
   binwidth = ui.input$filterPlot_binwidth
   type = ui.input$filterPlot_type
   facetGrid = ui.input$filterPlot_facetGrid
   scales = ui.input$filterPlot_scales
   
   x = data[, x.col]

   if (fill.col != "none"){
      fill = factor(data[, fill.col])
      
      p = data %>% 
         ggplot(., aes(fill = fill, col = fill)) +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]])
   } else {
      p = data %>% 
         ggplot(.) +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]])
   }
   
   

   
   if (type == "hist"){
      p = p +
         geom_histogram(mapping=aes(x = x), binwidth = binwidth) + #, fill = fill
         labs(x = labels[x.col][[1]])
      
   } 
   if (type == "freq"){
      p = p +
         geom_freqpoly(mapping=aes(x = x), binwidth = binwidth)  +
         
         labs(x = labels[x.col][[1]])
   }

   if (type == "boxp"){
      if (fill.col == "none"){
         y = 0
      } else {
         y = factor(data[, fill.col])
      }
      
      p = p +
         geom_boxplot(mapping=aes(y = x, x = y), alpha = 0.1)  + 
         labs(y = labels[x.col][[1]]) +
         theme(axis.title.x=element_blank())
   }
   
   if (type == "violin"){
      if (fill.col == "none"){
         y = 0
      } else {
         y = factor(data[, fill.col])
      }
      p = p +
         geom_violin(mapping=aes(y = x, x = y), alpha = 0.1)  + 
         stat_summary(mapping=aes(y = x, x = y),
                      fun.data=data_summary) +
         labs(y = labels[x.col][[1]]) +
         theme(axis.title.x=element_blank())
   }
   
   
   if (facetGrid){
      p = p +
         facet_grid(position ~ doy, labeller = label_both, scales = scales)
   }
   

   return(p)
}


######## TEMPERATURES ########

get.customizedPlotSettings = function(ui.input){
   return(list(
      x.col = ui.input$rawPlot.xcol,
      y.col = ui.input$rawPlot.ycol,
      col.col = ui.input$rawPlot.col,
      shape.col = ui.input$rawPlot.shape,
      facetWrap = ui.input$rawPlot_facetWrap,
      scales = ui.input$rawPlot_scales,
      facet = ui.input$rawPlot.facet,
      no.cols = ui.input$rawPlot.columns
   ))
}

plot.singleTemperature <- function(data, ui.input.processed){
   
   x.col = ui.input.processed$x.col
   y.col = ui.input.processed$y.col
   col.col = ui.input.processed$col.col
   shape.col = ui.input.processed$shape.col
   facetWrap = ui.input.processed$facetWrap
   scales = ui.input.processed$scales
   facet = ui.input.processed$facet
   no.cols = ui.input.processed$no.cols
   
   x = data[, x.col]
   y = data[, y.col]
   col = data[, col.col]
   shape = data[, shape.col]

   p = data %>% 
      ggplot(., aes(x = x, y = y, shape = factor(shape))) +
      labs(x = labels[x.col][[1]],
           y = labels[y.col][[1]],
           col = labels[col.col][[1]],
           shape = labels[shape.col][[1]]) 
   
   if (col.col == "dTime"){
      p = p +
         geom_point(aes(col = col)) +
         scale_color_gradient2(low = gradientcolors()[2], 
                               high = gradientcolors()[2], 
                               mid = gradientcolors()[1],
                               midpoint = 12)
      
      
   } else {
      p = p +
         geom_point(aes(col = factor(col))) +
         scale_color_manual(values = fillcolors(length(unique(col))))
   }
   
   if (facetWrap){
      facet = get.labelledFacets(data, facet) #facet.col
      p = p +
         facet_wrap(~ (facet), scales = scales,
                    ncol = no.cols)
   }
   
   if (length(unique(shape)) > 6){
      p = p +
         scale_shape_manual(values = c(1:length(unique(shape))))
   }
   return(p)
}

######## K-ESTIMATION ######## 

get.intersection <- function(data, y.col, x.col1, x.col2){
   m1 = lm(data[, y.col] ~ data[, x.col1], data = data)
   m2 = lm(data[, y.col] ~ data[, x.col2], data = data)
   a = coef(m1) - coef(m2)
   x = -a[[1]] / a[[2]]
   y = coef(m1)[[2]]*x + coef(m1)[[1]]
   return(c(x, y))
}

plot.nighttime <- function(data.complete){
   return(ggplot(data.complete, aes(x = dTime, y = dTsym.dTas,
                             col = doy, group = doy)) +
      # ylim(0, max(data.complete$dTsym.dTas)) +
      geom_hline(yintercept = 0., linetype = "dashed",  col = "#333333") +
      geom_line() +
      labs(x = labels["dTime"][[1]], 
           y = labels["dTsym.dTas"][[1]],
           col = labels["doy"][[1]]) 
      )
}

plot.kEst1 <- function(data.complete, data.adj, ui.input){
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fullrange = ui.input$k1Plot.fullrange
   fixedScales = ui.input$k1Plot_scales
   
   d = data.complete %>% 
      gather(., temp, value, dTsa, dTas, dTSym)
   ad = data.adj %>% 
      gather(., temp, value, dTsa, dTas)
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = dTsym.dTas, y = value, group = temp,
                             col = temp), shape = 1) +
      geom_point(ad, 
                 mapping = aes(x = dTsym.dTas, y = value, group = temp), 
                 col = "black", shape = 4) +
      stat_smooth(ad, method = "lm", formula = 'y ~ x',
                  mapping=aes(x = dTsym.dTas, y = value, group = temp),
                  col = "red") +
      stat_regline_equation(ad,
                            mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                        label =  paste(..eq.label.., ..adj.rr.label.., 
                                                       sep = "~~~~")),
                            label.y.npc = c("top", "bottom")) + #"center", 
      scale_color_manual(values=fillcolors(3)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]],
           caption = "* Black cross (x): data point used for regression") 
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2])
   }
   if (fullrange){
      p = p +
         stat_smooth(ad, method = "lm", 
                     mapping=aes(x = dTsym.dTas, y = value, group = temp),
                     col = "#333333", fullrange = T, se = F,
                     size = 0.5)
   }
   return(p)
}

plot.kEst2 <- function(data.complete, data.adj, k, 
                       ui.input){
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fullrange = ui.input$k1Plot.fullrange
   fixedScales = ui.input$k1Plot_scales
   force = ui.input$k1Plot.forceOrigin
   
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   fit = ifelse(force, "y ~ x + 0", "y ~ x")
   
   d = data.complete %>% 
      mutate("K+dTsa" = (dTsa + k)) %>% 
      gather(., temp, value, dTsa, dTas, dTSym, `K+dTsa`)

   newAdj = data.adj %>% 
      mutate("K+dTsa" = (dTsa + k))%>% 
      gather(., temp, value, dTSym, `K+dTsa`) #dTsa, dTas, 

   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = dTsym.dTas, y = value, group = temp,
                             col = temp), shape = 1) +
      geom_point(newAdj, 
                 mapping=aes(x = dTsym.dTas, y = value), 
                 shape = 4) +
      stat_smooth(newAdj, method = "lm", formula = fit,
                  mapping=aes(x = dTsym.dTas, y = value, group = temp,
                              col = temp),
                  col = "red", size = 0.5,
                  fullrange = T, se = F) +
      stat_regline_equation(newAdj,
                            formula = fit, #force through origin x+0
                            mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                        label =  ..adj.rr.label..),
                            label.y.npc = c("top", "bottom")) + 
      geom_label(aes(x = 0.9 * max(d$dTsym.dTas), y = 0.9 * max(d$value),
                     label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6) +
      scale_color_manual(values=fillcolors(4)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      ylim(min(d$value), max(d$value)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      geom_hline(yintercept = 0, linetype = "dashed", col = "#333333") +
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]],
           caption = "* Black cross (x): data point used for regression")
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2]) +
         geom_label(aes(x = 0.9 * xRange[2], y = 0.9 * max(d$value),
                        label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6)
   }
   if (fullrange){
      p = p +
         stat_smooth(data.adj, method = "lm", 
                     mapping=aes(x = dTsym.dTas, y = dTas),
                     col = "#333333", fullrange = T, se = F,
                     size = 0.5)
   }
   return(p)
}

plot.kEst3 <- function(data.complete, data.adj, k,
                       ui.input){
   xRange = c(ui.input$k1Plot.x.min, ui.input$k1Plot.x.max)
   fixedScales = ui.input$k1Plot_scales
   
   d = data.complete %>%
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   newAdj = data.adj %>% 
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   if (min(data.complete$dTsym.dTas, na.rm = T) < 0){
      xmin = min(data.complete$dTsym.dTas, na.rm = T)
   } else {
      xmin = -0.1
   }
   
   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTas, 
                             col = x.temp, shape = "dTas")) +
      geom_point(newAdj, 
                 mapping=aes(x = x.value, y = dTas), 
                 shape = 4) +
      stat_smooth(newAdj, method = "lm", formula = 'y ~ x',
                  mapping=aes(x = x.value, y = dTas,
                              col = x.temp, group = x.temp),
                  col = "red", size = 0.5,
                  fullrange = T, se = F) +
      
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTsa, 
                             col = x.temp, shape = "dTsa")) +
      geom_point(newAdj, 
                 mapping=aes(x = x.value, y = dTsa), 
                 shape = 4) +
      stat_smooth(newAdj, method = "lm", formula = 'y ~ x',
                  mapping=aes(x = x.value, y = dTsa,
                              col = x.temp, group = x.temp),
                  col = "red", size = 0.5,
                  fullrange = T, se = F) +
      
      geom_label(aes(x = 0.9 * max(d$x.value), y = 0.9 * max(d$dTas),
                     label = paste("k = ", round(k, 2))), 
                 fill = "#B8B361", alpha = 0.6) + #D2D0AD
      scale_color_manual(values=fillcolors(2)) +
      scale_shape_manual(values = c(21, 24)) +
      xlim(xmin, max(d$x.value)) +
      ylim(-max(d$dTas), max(d$dTas)) +
      
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      
      labs( x = "dTsym /dTas | R = (k + dTsa) / dTas", 
            y = labels["dT"][[1]], 
            col = "x-axis", 
            shape = labels["T"][[1]],
            caption = "* Black cross (x): data point used for regression.
            Gray-shaded values indicate the point of intersection of the two lines.")
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2]) +
         geom_label(aes(x = 0.9 * xRange[2], y = 0.9 * max(d$dTas),
                        label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6)
   }

   return(p)
}

######## SAP FLOW INDEX ########

plot.sapFlowIndex <- function(data, ui.input){
   scales = ui.input$sfIndexPlot_scales
   facetWrap = ui.input$sfIndexPlot_wrap
   facet.col = ui.input$sfIndexPlot.facet
   facet.col.no = ui.input$sfIndexPlot.facet.cols

   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = datetime, y = dTSym, col = factor(position))) +
      labs(x = "", 
           y = labels["SFI"][[1]],
           col = labels["position"][[1]])
   
   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = data %>% 
         ggplot(aes(x = dTime, y = dTSym))
      
      if (facet.col == "position"){
         p = p +
            geom_line(aes(col = factor(doy))) +
            labs(x = labels["dTime"][[1]], 
                 y = labels["SFI"][[1]],
                 col = labels["doy"][[1]])
      }
      if (facet.col == "doy"){
         p = p +
            geom_line(aes(col = factor(position))) +
            labs(x = labels["dTime"][[1]], 
                 y = labels["SFI"][[1]],
                 col = labels["position"][[1]])
      }
      p = p  +
         facet_wrap(~ (facet), scales = scales,
                    ncol = facet.col.no)
      
   }
   return(p)
}


######## SAP FLOW DENSITY ########

plot.emptyMessage = function(message){
   return(p = ggplot() +
             annotate(geom = "text", x = 5, y = 5, 
                      label = message,
                      color = "red", size = 8) +
             theme_void())
}

plot.sapFlowDensity.Helper = function(data, ui.input, boxplot = F){
   # check if sap flow density is Inf
   # helper variable; if 0 no sap flow density data is avail.
   SFDensity = nrow(data)
   if (ui.input$sapFlowDensityPlot.y == "SFDsw"){
      SFDensity = nrow(data %>% 
                          mutate(all = n()) %>% 
                          filter(abs(SFDsw) != Inf))
   }

   # show error message if sap flow density haven't been calculated (i.e. is Inf)
   if (SFDensity == 0){
      p = plot.emptyMessage(message = "Wood properties are missing.")
   } else{
      p = plot.sapFlowDensity(data = data, 
                              ui.input = ui.input,
                              boxplot = boxplot) 
   }
   return(p)
}


plot.sapFlowDensity <- function(data, ui.input, boxplot = F){
   scales = ui.input$sapFlowDensityPlot_scales
   facetWrap = ui.input$sapFlowDensityPlot_facetWrap
   facet.col = ui.input$sapFlowDensityPlot.facet
   facet.col.no = ui.input$sapFlowDensityPlot_facet.cols
   
   y = ui.input$sapFlowDensityPlot.y
   col = ui.input$sapFlowDensityPlot.color
   y.col = data[, y]
   
   col = ifelse(facet.col == "position", "doy", "position")
   col.col = data[, col]

   if (boxplot){
      p = data %>% 
         ggplot(.) +
         geom_boxplot(aes(x = factor(position), y = y.col,
                          col = factor(position))) +
         labs(x = labels["position"][[1]], 
              y = labels[y][[1]],
              col = labels["position"][[1]])
      if (facetWrap){
         p = p +
            facet_wrap(~ doy, scales = scales,
                       ncol = facet.col.no)
      }
      
   } else {
      p = data %>% 
         ggplot(.) +
         geom_line(aes(x = datetime, y = y.col, col = factor(col.col))) +
         labs(x = "", 
              y = labels[y][[1]],
              col = labels[col][[1]])
      if (facetWrap){
         facet = get.labelledFacets(data, facet.col)
         p = data %>% 
            ggplot(aes(x = dTime, y = y.col)) +
            geom_line(aes(col = factor(col.col))) +
            labs(x = labels["dTime"][[1]], 
                 y = labels[y][[1]],
                 col = labels[col][[1]]) +
            facet_wrap(~ (facet), scales = scales,
                       ncol = facet.col.no)
      }
   }
   return(p)
}

######## SAP FLOW RATE ########

get.selectedMethods = function(ui.input){
   groups = c()
   if (ui.input$treeScaleSimple1){
      groups = rbind(groups, "sfM1")
   }
   if (ui.input$treeScaleSimple2){
      groups = rbind(groups, "sfM2")
   }
   if (ui.input$treeScaleSimple3){
      groups = rbind(groups, "sfM3")
   }
   
   groups = groups[,1]
   return(groups)
}

plot.sapFLowRate = function(data, ui.input){
   N = 0
   p = data %>% 
      ggplot(.) +
      labs(x = "",
           y = labels["SF"][[1]],
           color = "Scaling method",
           linetype = "Scaling method")
   if (ui.input$treeScaleSimple1){
      p = p +
         geom_line(aes(x = datetime, y = sfM1, color = "Method 1",
                       linetype = "Method 1"),
                   size = 0.9)
      N = N + 1
   }
   if (ui.input$treeScaleSimple2){
      p = p +
         geom_line(aes(x = datetime, y = sfM2, color = "Method 2",
                       linetype = "Method 2"),
                   size = 0.9)
      N = N + 1
   }
   if (ui.input$treeScaleSimple3){
      p = p +
         geom_line(aes(x = datetime, y = sfM3, color = "Method 3",
                       linetype = "Method 3"),
                   size = 0.9)
      N = N + 1
   }
   p = p +
      scale_color_manual(values = fillcolors(N))
   
   return(p)
}

plot.sapFlowDay = function(data, ui.input){
   groups = c()
   if (ui.input$treeScaleSimple1){
      groups = rbind(groups, "sfM1")
   }
   if (ui.input$treeScaleSimple2){
      groups = rbind(groups, "sfM2")
   }
   if (ui.input$treeScaleSimple3){
      groups = rbind(groups, "sfM3")
   }
   
   groups = groups[,1]
   
   data = data %>% 
      gather(., Method, SFrate, groups) %>% 
      mutate(Method = ifelse(Method == "sfM1", "Method 1",
                             ifelse(Method == "sfM2", "Method 2",
                                    "Method 3")),
             Balance = ifelse(SFrate >= 0, "Positive", "Negative")) %>% 
      mutate(Balance = factor(Balance, levels = c("Positive", "Negative"))) %>% 
      filter(complete.cases(.)) %>% 
      group_by(doy, Method, Balance) %>% 
      arrange(dTime) %>% 
      distinct(auc = sum(diff(dTime) * (head(SFrate,-1)+tail(SFrate,-1)))/2) 
   return(data %>% 
      ggplot(., aes(x = factor(doy), fill = Method, y = auc)) +
         geom_bar(position="dodge", stat="identity", 
                  col = "black", alpha = 0.6) +
         scale_fill_manual(values = fillcolors(length(groups))) +
         labs(x = labels["doy"][[1]],
              y = "Area under curve (kg)",
              fill = "Scaling method"))
}

