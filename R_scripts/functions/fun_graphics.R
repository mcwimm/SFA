fillcolors = function(N){
   col = c("#d8b365", "#260C7D", "#5ab4ac", 
           "#7D410C", "#007D06",
           '#999999','#E69F00', '#56B4E9')
   return(col[1:N])
}


gradientcolors <- function(){
   # return(c("#AED4E6", "#03354D"))
   print("COLORRRRRRR")
   # if (is.null(input$gradientColorLow)){
   col = c("#d8b365", "#5ab4ac")
   # } else {
   #    col = c(input$gradientColorLow, input$gradientColorHigh)
   #
   # }
   print(col)
   return(col)
}

get.labelledFacets = function(data, facet.col){
   facet = as.integer(data[, facet.col]) 
   facet.factor <- c(unique(facet))
   labs = unlist(lapply(facet.factor, function(x) paste(facet.col, ": ", x, sep = "")))
   return(factor(facet, labels = labs))
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
               "SFS" = expression(paste("Sap Flow per Section (g \u22C5 ", cm^-1, "\u22C5", h^-1, ")")),
               "SFDsw" = expression(paste("Sapwood-related Sap Flow Density (g \u22C5", cm^-2, "\u22C5", h^-1, ")")))


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


######## FILTER ########

plot.histogram <- function(data, x.col, fill.col, binwidth = 0.1,
                           type, facetGrid = T, scales){
   
   x = data[, x.col]

   if (fill.col != "none"){
      fill = factor(data[, fill.col])
      
      p = data %>% 
         ggplot(., aes(fill = fill, col = fill)) +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]]) +
         theme_bw()
   } else {
      p = data %>% 
         ggplot(.) +
         labs(fill = labels[fill.col][[1]],
              col = labels[fill.col][[1]]) +
         theme_bw()
   }
   
   

   
   if (type == "hist"){
      p = p +
         geom_histogram(mapping=aes(x = x), binwidth = binwidth) + #, fill = fill
         labs(x = labels[x.col][[1]])
      
   } 
   if (type == "freq"){
      p = p +
         # geom_freqpoly(mapping=aes(x = x, col = fill), binwidth = binwidth)  +
         geom_freqpoly(mapping=aes(x = x), binwidth = binwidth)  +
         
         labs(x = labels[x.col][[1]])
   }
   
   if (type == "boxp"){
      p = p +
         geom_boxplot(mapping=aes(y = x), alpha = 0.1)  + #, group = fill, col = fill, fill = fill
         labs(y = labels[x.col][[1]]) +
         theme(axis.title.x=element_blank(),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank())
   }
   
   
   if (facetGrid){
      p = p +
         facet_grid(position ~ doy, labeller = label_both, scales = scales)
   }
   

   return(p)
}


######## TEMPERATURES ########



plot.deltaTfacetWrap <- function(data, xRange, yRange, 
                                 scales, facetWrap = T, facet.col){

   p = data %>% 
      gather(., key, value, "dTas", "dTsa", "dTSym") %>% 
      ggplot(.) +
      geom_line(mapping=aes(x = dTsym.dTas, y = value, 
                            col = factor(key), 
                            group = factor(key))) +
      scale_color_manual(values = fillcolors(3)) +
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]],
           col = labels["T"][[1]]) +
      theme_bw()

   
   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = p +
         facet_wrap(~ (facet), scales = scales)
   }
   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
         
   }
   return(p)
}

plot.singleTemperature <- function(data, x.col, y.col, col.col, shape.col, facetWrap,
                                   xRange, yRange, scales, facet.col){

   x = data[, x.col]
   y = data[, y.col]
   col = data[, col.col]
   shape = data[, shape.col]

   p = data %>% 
      ggplot(., aes(x = x, y = y, shape = factor(shape))) +
      labs(x = labels[x.col][[1]],
           y = labels[y.col][[1]],
           col = labels[col.col][[1]],
           shape = labels[shape.col][[1]]) +
      theme_bw()
   
   if (col.col == "dTime"){
      p = p +
         geom_point(aes(col = col)) +
         scale_color_gradient(low = gradientcolors()[1], high = gradientcolors()[2])
      
      
   } else {
      p = p +
         geom_point(aes(col = factor(col))) +
         scale_color_manual(values = fillcolors(length(unique(col))))
   }
   
   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = p +
         facet_wrap(~ (facet), scales = scales)
   }
   
   if (length(unique(shape)) > 6){
      p = p +
         scale_shape_manual(values = c(1:length(unique(shape))))
   }
   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
      
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
      labs(x = "Time (h)", y = labels["dTsym.dTas"][[1]]) +
      theme_bw())
}

plot.kEst1 <- function(data.complete, data.adj, xRange, fullrange = F, fixedScales = T){
   d = data.complete %>% 
      gather(., temp, value, dTsa, dTas, dTSym)
   ad = data.adj %>% 
      gather(., temp, value, dTsa, dTas)
   
   if (min(data.complete$dTsym.dTas < 0)){
      xmin = min(data.complete$dTsym.dTas < 0)
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
      stat_smooth(ad, method = "lm", 
                  mapping=aes(x = dTsym.dTas, y = value, group = temp),
                  col = "red") +
      stat_regline_equation(ad,
                            mapping=aes(x = dTsym.dTas, y = value, group = temp,
                                        label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
                            label.y.npc = c("top", "bottom")) + #"center", 
      scale_color_manual(values=fillcolors(3)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]],
           caption = "* Black cross (x): data point used for regression") +
      theme_bw()
   
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
                       xRange, fullrange = F, fixedScales = T, force = T){
   
   if (min(data.complete$dTsym.dTas < 0)){
      xmin = min(data.complete$dTsym.dTas < 0)
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
                     label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6) + #D2D0AD
      scale_color_manual(values=fillcolors(4)) +
      xlim(xmin, max(d$dTsym.dTas)) +
      ylim(min(d$value), max(d$value)) +
      geom_vline(xintercept = 0, linetype = "dashed", col = "#333333") +
      geom_hline(yintercept = 0, linetype = "dashed", col = "#333333") +
      labs(x = labels["dTsym.dTas"][[1]], 
           y = labels["dT"][[1]], 
           col = labels["T"][[1]],
           caption = "* Black cross (x): data point used for regression") +
      theme_bw()
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2])
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
                       xRange, fixedScales = T){
   d = data.complete %>%
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   newAdj = data.adj %>% 
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) 
   names(newAdj)
   int1 = round(get.intersection(newAdj, "dTas", "dTsym.dTas", "R = (k + dTsa) / dTas"), 2)
   int2 = round(get.intersection(newAdj, "dTsa", "dTsym.dTas", "R = (k + dTsa) / dTas"), 2)

   newAdj = data.adj %>% 
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   if (min(data.complete$dTsym.dTas < 0)){
      xmin = min(data.complete$dTsym.dTas < 0)
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
      stat_smooth(newAdj, method = "lm",
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
      stat_smooth(newAdj, method = "lm",
                  mapping=aes(x = x.value, y = dTsa,
                              col = x.temp, group = x.temp),
                  col = "red", size = 0.5,
                  fullrange = T, se = F) +
      
      geom_label(aes(x = 0.9 * max(d$x.value), y = 0.9 * max(d$dTas),
                     label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6) + #D2D0AD
      geom_label(aes(x = 0, y = int1[2]*1.15,
                     label = paste(int1[1], int1[2], sep = " | ")), 
                     fill = "#C9C9C9", alpha = 0.5) +
      geom_label(aes(x = 0, y = int2[2]*1.15,
                     label = paste(int2[1], int2[2], sep = " | ")), 
                 fill = "#C9C9C9", alpha = 0.5) +
      
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
            Gray-shaded values indicate the point of intersection of the two lines.") +
      theme_bw()
   
   if (fixedScales){
      p = p +
         xlim(xRange[1], xRange[2])
   }

   return(p)
}

######## SAP FLOW INDEX ########

plot.sapFlowIndex = function(data, yRange, scales, facetWrap, facet.col){

   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = datetime, y = dTSym, col = factor(position))) +
      labs(x = "", 
           y = labels["SFI"][[1]],
           col = labels["position"][[1]]) +
      theme_bw()
   
   # if (facetWrap){
   #    # timelist.minor = seq(from = min(data[data$dTime == 6, ]$datetime),
   #    #                      to = max(data[data$dTime == 6, ]$datetime) + 1,
   #    #                      by = "day")
   #    # timelist = seq(from = min(data[data$dTime == 12, ]$datetime),
   #    #                to = max(data[data$dTime == 12, ]$datetime) + 1,
   #    #                by = "day")
   #    # print(timelist)
   #    
   #    
   #    p = p +
   #       facet_wrap(~ position, labeller = label_both, scales = scales) +
   #       scale_x_datetime(#minor_breaks = date_breaks("6 hours"), 
   #                        # breaks = timelist,
   #                        labels = date_format("%d-%m\n%H:%M"))
   #    
   # }
   
   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = p +
         facet_wrap(~ (facet), scales = scales)
   }
   
   
   if (scales == "fixed"){
      p = p +
         ylim(yRange[1], yRange[2]) 
   }
   
  
   return(p)
}



plot.sapFlowIndex.Day = function(data, xRange, yRange, scales, facetWrap, facet.col){
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = dTime, y = dTSym, group = factor(doy), col = factor(doy))) +
      labs(x = labels["dTime"][[1]], 
           col = labels["doy"][[1]],
           y = labels["SFI"][[1]]) +
      theme_bw() 
   
   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = p +
         facet_wrap(~ (facet), scales = scales)
   }
   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2])
   }

   return(p)
}


######## SAP FLOW DENSITY ########

plot.sapFlowDensity <- function(data, 
                                y,
                                col, 
                                scales, 
                                facetWrap = T, facet.col){
   y.col = data[, y]
   col.col = factor(data[, col])
   
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = dTime, y = y.col, col = col.col)) +
      labs(x = labels["dTime"][[1]], 
           col = labels[col][[1]],
           y = labels[y][[1]]) +
      theme_bw() 
   

   if (facetWrap){
      facet = get.labelledFacets(data, facet.col)
      p = p +
         facet_wrap(~ (facet), scales = scales)
   }
   
   return(p)
}


######## SAP FLOW RATE ########

plot.sapFLowRate = function(data, input){
   
   p = data %>% 
      ggplot(.) +
      labs(x = "",
           y = "Sap flow rate (kg/h)",
           color = "Scaling method")
   if (input$treeScaleSimple1){
      p = p +
         geom_line(aes(x = datetime, y = sfM1, color = "method 1"))
   }
   if (input$treeScaleSimple2){
      p = p +
         geom_line(aes(x = datetime, y = sfM2, color = "method 2"))
   }
   if (input$treeScaleSimple3){
      p = p +
         geom_line(aes(x = datetime, y = sfM3, color = "method 3"))
   }
   return(p)
}

