# https://cran.r-project.org/web/packages/document/vignettes/Introduction_to_document.html
# https://cran.r-project.org/web/packages/document/document.pdf

path = "./R_scripts/functions/"
filenames = list.files(path)
filenames = filenames[1:4]
output_directory = "./man/"
############ apply to one function file

filename = filenames[1]
# cat(readLines(paste(path, filename, sep = "")), sep = "\n")
d <- document::document(paste(path, filename, sep = ""), 
                        check_package = FALSE)

############
d <- document::document(file_name = "./R_scripts/functions/fun_data.R", 
                        check_package = FALSE)

####### copy pdf files from temp directory to ./man
file.copy(d[["pdf_path"]], output_directory)



############ apply to multiple function files

doc_func <- function(filename){
   return(document::document(paste(path, filename, sep = ""), 
                             check_package = FALSE))
   
}

res = lapply(filenames[-1], doc_func)
d = unlist(lapply(c(1:length(res)), function(x) res[[x]]["pdf_path"]))

####### copy pdf files from temp directory to ./man

file.copy(d, output_directory, overwrite = T)


