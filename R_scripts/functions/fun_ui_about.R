
#############
### ABOUT ###
#############

introOutput = function(){
   return(list(
      fluidRow(
         box(title = "Sap Flow Analyzer",
             status = "info", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_rm.md")),
         box(title = "Guide",
             status = "warning", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_guide.md"))  
      ),
      fluidRow(
         box(title = "Heat Field Deformation Method",
             status = "primary", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_meth.md"),
             img(src='hfd_principle.png', width = "100%")
         ),
         box(title = "Outputs",
             status = "success", solidHeader = F, #height = 300,
             collapsible = T,
             includeMarkdown("./man/des_main_out.md"))
      )
   ))   
}



