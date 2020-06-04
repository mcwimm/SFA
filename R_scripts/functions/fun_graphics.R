fillcolors = function(N){
   col = c("#d8b365", "#260C7D", "#5ab4ac", 
           "#7D410C", "#007D06",
           '#999999','#E69F00', '#56B4E9')
   return(col[1:N])
}

######## TEMPERATURES ########

plot.deltaTemperature <- function(data, yRangeSlider){
   return(data %>% 
             ggplot(.) +
             geom_line(mapping=aes(x = timestep, y = dTSym, 
                                   col = factor(depth), group = factor(depth))) +
             # scale_x_datetime() +
             ylim(yRangeSlider[1], yRangeSlider[2]) +
             labs(x = "", 
                  col = "Depth") +
             theme_bw())
}

plot.deltaTfacetWrap <- function(data, xRange, yRange, free){
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
           col = "Temperature (\u00B0 C)") +
      theme_bw()

   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
         
   }
   return(p)
}

######## K-ESTIMATION ######## 

plot.kEst1 <- function(data.complete, data.adj, xRange, fullrange = F, fixedScales = T){
   d = data.complete %>% 
      gather(., temp, value, dTsa, dTas, dTSym)
   ad = data.adj %>% 
      gather(., temp, value, dTsa, dTas)
   
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
      labs(x = "dTsym / dTas", y = "T (\u00B0 C)", col = "Temperature (\u00B0 C)") +
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

plot.kEst2 <- function(data.complete, data.adj, k, xRange, fullrange = F, fixedScales = T){
   d = data.complete %>% 
      mutate("K+dTsa" = (dTsa + k)) %>% 
      gather(., temp, value, dTsa, dTas, dTSym, `K+dTsa`)

   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = dTsym.dTas, y = value, group = temp,
                             col = temp), shape = 1) +
      stat_smooth(data.adj, method = "lm", 
                  mapping=aes(x = dTsym.dTas, y = dTas),
                  col = "red") +
      geom_label(aes(x = 0.9 * max(d$dTsym.dTas), y = 0.9 * max(d$value),
                     label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6) + #D2D0AD
      scale_color_manual(values=fillcolors(4)) +
      labs(x = "dTsym / dTas", y = "T (\u00B0 C)", col = "Temperature (\u00B0 C)") +
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

plot.kEst3 <- function(data.complete, data.adj, k){
   d = data.complete %>%
      mutate(`R = (k + dTsa) / dTas` = (k + dTsa) / dTas) %>% 
      gather(., x.temp, x.value, `dTsym.dTas`, `R = (k + dTsa) / dTas`)
   
   p = ggplot() +
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTas, 
                             col = x.temp, shape = "dTas")) +
      geom_point(d, 
                 mapping=aes(x = x.value, y = dTsa, 
                             col = x.temp, shape = "dTsa")) +
      scale_x_continuous("dTsa / dTas",
                         sec.axis = sec_axis(~ . , name = "R = (k + dTsa) / dTas")) +
      geom_label(aes(x = 0.9 * max(d$x.value), y = 0.9 * max(d$dTas),
                     label = paste("k = ", round(k, 2))), fill = "#B8B361", alpha = 0.6) + #D2D0AD
      scale_color_manual(values=fillcolors(2)) +
      scale_shape_manual(values = c(21, 24)) +
      labs( y = "T (\u00B0 C)", col = "x-axis", shape = "Temperature (\u00B0 C)") +
      theme_bw()
   
   return(p)
}

######## SAP FLOW INDEX ########

plot.sapFlowIndex = function(data, yRange, free, wrap){
   scales = ifelse(free, "free", "fixed")
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = datetime, y = dTSym, col = factor(depth))) +
      labs(x = "", 
           y = "Sap Flow Index (dTSym)",
           col = "Sensor depth") +
      theme_bw()
   
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

plot.sapFlowIndex.Day = function(data, xRange, yRange, free){
   scales = ifelse(free, "free", "fixed")
   
   p = data %>% 
      ggplot(.) +
      geom_line(aes(x = dTime, y = dTSym, group = doy, col = doy)) +
      facet_wrap(~ depth, labeller = label_both, scales = scales) +
      labs(x = "Time (h)", 
           col = "Day of year",
           y = "Sap Flow Index (dTSym)") +
      theme_bw() 
   
   if (scales == "fixed"){
      p = p +
         xlim(xRange[1], xRange[2]) +
         ylim(yRange[1], yRange[2]) 
   }
   return(p)
}