get.sapFlowDensity <- function(method = "HFD", data, Dst, Zax, Ztg,
                               sapWoodDepth){
   if (method == "HFD"){
      data$SFS = 3600 * Dst * (data[, "k"] + data[, "dTsa"]) / data[, "dTas"] *
         Zax / Ztg
      data$SFDsw = NA
      data$SFDsw = data$SFS / sapWoodDepth
      
   }
   return(data)
}
