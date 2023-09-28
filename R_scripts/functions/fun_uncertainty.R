######## UNCERTAINTY ########

get.sapFlowDensitySA <- function(data, f_k, f_D, f_Z, f_swd){
   data$SFS = 3600 * (data$Dst*f_D) * ((data[, "k"]*f_k) + data[, "dTsa"]) / data[, "dTas"] *
      (data$Zax / data$Ztg*f_Z)
   if ("swd" %in% colnames(data)){
      data$SFDsw = data$SFS / (data$swd*f_swd)
   }
   return(data)
}

get.uncertainty = function(data, depths, ui.input){
   r = ui.input$uncertaintyRange
   vs = 1 - seq(-r, r, length.out = ui.input$uncertaintySteps) / 100
   vs = sort(unique(c(vs, 1)))
   
   df_uncert = data.frame()
   for (param in c("Z", "L", "D", "k")) {
      f_k = 1
      f_D = 1
      f_Z = 1
      f_swd = 1
      for (v in vs) {
         if (param == "Z") {
            f_Z = v
         }
         if (param == "L") {
            f_swd = v
         }
         if (param == "D") {
            f_D = v
         }
         if (param == "k") {
            f_k = v
         }
         d = get.sapFlowDensitySA(
            data = data,
            f_k = f_k,
            f_D = f_D,
            f_Z = f_Z,
            f_swd = f_swd
         )
         
         if (ui.input$uncert_y == "SFS") {
            t = d %>%
               ungroup() %>%
               reframe(y = mean(SFS, na.rm = F))
         }
         if (ui.input$uncert_y == "SFD") {
            if ("SFDsw" %in% colnames(d)) {
               t = d %>%
                  ungroup() %>%
                  reframe(y = mean(SFDsw, na.rm = F))
            } else {
               return(data.frame())
            }
         }
         
         if (ui.input$uncert_y == "SF"){
            d = d %>% 
               merge(., depths, by = "position")
            if ("Cring" %in% colnames(d)){
               t = get.sapFlowByMethod(data = d,
                                       method = ui.input$uncert_y_method, 
                                       swd = unique(d$swd),
                                       ui.input = ui.input)
               
               if (ui.input$uncert_y_method == "treeScaleSimple1"){
                  t$sfM = t$sfM1
               } 
               if (ui.input$uncert_y_method == "treeScaleSimple2"){
                  t$sfM = t$sfM2
               } 
               if (ui.input$uncert_y_method == "treeScaleSimple3"){
                  t$sfM = t$sfM3
               } 
               t = t %>%
                  ungroup() %>% 
                  reframe(y = mean(sfM, na.rm = F))
            } else {
               return(data.frame())
            }
         }
         if (ui.input$uncert_y == "TWU"){
            d = d %>% 
               merge(., depths, by = "position")
            if ("Cring" %in% colnames(d)){
               t = get.sapFlowByMethod(data = d,
                                       method = ui.input$uncert_y_method, 
                                       swd = unique(d$swd),
                                       ui.input = ui.input)
               t = get.treeWaterUseByMethod(t, ui.input, method = T)
               t = t %>%
                  ungroup() %>%
                  reframe(y = mean(Positive, na.rm = F))
            } else {
               return(data.frame())
            }
         }
         df_uncert = bind_rows(df_uncert, t %>% 
                                  mutate(parameter=param,
                                         param.value = v))
      }
   }
   
   ref = df_uncert %>% 
      filter(param.value == 1) %>% 
      distinct(y)
   
   
   df_uncert = df_uncert %>% 
      mutate(y_ref = (y - ref[[1]])/ref[[1]] * 100)
   
   
   return(df_uncert)
}


get.uncertTable <- function(values, uncertaintyValues, absolute=T){
   if (!is.null(values$kvalues)){
      uncert = uncertaintyValues
      if (!absolute){
         uncert$y = uncert$y_ref
      }
      uncert$y_ref = NULL
      if (nrow(uncert) > 0 & sum(uncert$y) != 0){
         if ("L" %in% uncert$parameter){
            return(uncert %>% 
                      mutate(parameter = factor(parameter,
                                                levels = c("D", "Z", "L", "k"),
                                                labels = c("Dnom", "Zax/Ztg", "Lsw", "k"))) %>% 
                      spread(., parameter, y) %>% 
                      mutate_at(colnames(.)[-1], round, 2) %>%
                      mutate(param.value = round((param.value-1)*100)) %>% 
                      rename("Error (%)" = "param.value")
            )
         } else {
            return(uncert %>% 
                      mutate(parameter = factor(parameter,
                                                levels = c("D", "Z", "k"),
                                                labels = c("Dnom", "Zax/Ztg", "k"))) %>% 
                      spread(., parameter, y) %>% 
                      mutate_at(colnames(.)[-1], round, 2) %>%
                      mutate(param.value = round((param.value-1)*100)) %>% 
                      rename("Error (%)" = "param.value")
            )
         }
      }
   }
}
