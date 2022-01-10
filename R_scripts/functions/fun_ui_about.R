
#############
### ABOUT ###
#############

introOutput = function(){
   return(
      fluidRow(
         column(6,
                box(title = "Sap Flow Analyzer",
                    status = "info", solidHeader = F, width = "100%",
                    collapsible = T,
                    includeMarkdown("./man/des_main_rm.md")),
                box(title = "Heat Field Deformation Method",
                    status = "primary", solidHeader = F, width = "100%",
                    collapsible = T,
                    includeMarkdown("./man/des_main_meth.md"),
                    img(src='hfd_principle.png', width = "100%"))
                ),
         column(6,
                box(title = "Guide",
                    status = "warning", solidHeader = F, width = "100%",
                    collapsible = T,
                    includeMarkdown("./man/des_main_guide.md")),
                box(title = "Outputs",
                    status = "success", solidHeader = F, width = "100%",
                    collapsible = T,
                    includeMarkdown("./man/des_main_out.md"))
                )
   ))   
}



