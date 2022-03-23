
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
                    status = "info", solidHeader = F, width = "100%",
                    collapsible = T,
                    includeMarkdown("./man/des_main_meth.md"),
                    
                    p(HTML('&nbsp;'),
                      a(HTML("&Ccaron;erm&aacute;k (2004)"), 
                        href="https://link.springer.com/article/10.1007/s00468-004-0339-6", target="_blank"), ",",
                      HTML('&nbsp;'),
                      a("Nadezhdina (2018)", 
                        href="https://iforest.sisef.org/abstract/?id=ifor2381-011", target="_blank"), ""),
                    
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



