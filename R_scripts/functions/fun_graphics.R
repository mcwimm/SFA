fillcolors = function(N){
   col = c("#d8b365", "#260C7D", "#5ab4ac", 
         "#260C7D", "#007D06", "#7D410C")
   return(col[1:N])
}

######## TEMPERATURES

plot.deltaTemperature <- function(data, yRangeSlider, fn){
   return(data %>% 
             ggplot(.) +
             geom_line(mapping=aes(x = timestep, y = dTSym, 
                                   col = factor(depth), group = factor(depth))) +
             # scale_x_datetime() +
             ylim(yRangeSlider[1], yRangeSlider[2]) +
             labs(x = "", 
                  col = "Depth") +
             theme_bw() +   
             ggtitle(fn))
}

plot.deltaTfacetWrap <- function(data, xRange, yRange, free, fn){
   fillcolors = c("#d8b365", "#260C7D", "#5ab4ac") # "#f5f5f5", 
   # fillcolors = c("#260C7D", "#007D06", "#7D410C")
   scales = ifelse(free, "free", "fixed")
   p = data %>% 
      gather(., key, value, "dTas", "dTsa", "dTSym") %>% 
      ggplot(.) +
      geom_line(mapping=aes(x = dTsym.dTas, y = value, 
                            col = factor(key), 
                            group = factor(key))) +
      scale_color_manual(values = fillcolors) +
      facet_wrap(~ depth, labeller = label_both, scales = scales) +
      labs(x = "dTsym.dTas", 
           y = "dT",
           col = "Temperature diff.") +
      theme_bw() +   
      ggtitle(fn)
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
         
   }
   return(p)
}

######## K-Estimation

plot.kEst1 <- function(data.complete, data.adj, x.min, x.max, 
                       fullrange = F){
   d = data.complete %>% 
      gather(., temp, value, dTsa, dTas, dTSym)
   ad = data.adj %>% 
      gather(., temp, value, dTsa, dTas)#, dTSym)
   
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
      xlim(x.min, x.max) +
      labs(x = "dTsym / dTas (C)", y = "T (C)", col = "") +
      theme_bw()
   
   if (fullrange){
      p = p +
         stat_smooth(ad, method = "lm", 
                     mapping=aes(x = dTsym.dTas, y = value, group = temp),
                     col = "#333333", fullrange = T, se = F,
                     size = 0.5)
   }
   return(p)
}

######## SAP FLOW INDEX

plot.sapFlowIndex = function(data, yRange, free, wrap, fn){
   scales = ifelse(free, "free", "fixed")
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = datetime, y = dTSym, col = factor(depth))) +
      labs(x = "", 
           y = "Sap Flow Index (dTSym)",
           col = "Sensor depth") +
      theme_bw() +
      ggtitle(fn)
   
   if (wrap){
      p = p +
         facet_wrap(~ depth, labeller = label_both, scales = scales)
   }
   if (scales == "fixed"){
      p = p +
         ylim(yRange[1], yRange[2]) 
   }
   return(p)
}

plot.sapFlowIndex.Day = function(data, xRange, yRange, free, fn){
   scales = ifelse(free, "free", "fixed")
   
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = dTime, y = dTSym, group = doy, col = doy)) +
      facet_wrap(~ depth, labeller = label_both, scales = scales) +
      labs(x = "Time (h)", 
           col = "Day of year",
           y = "Sap Flow Index (dTSym)") +
      theme_bw() +
      ggtitle(fn)
   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
   }
   return(p)
}