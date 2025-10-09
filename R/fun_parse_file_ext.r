#' @title Parses file extension
#'
#' @description Parses file extension from the provided file name.
#'
#' @param file.name string original image file name.
#'
#' @return string parsed file extension.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-09 / Frt
# - `Created`    : 2025-10-09 / Frt
# - `Last test`  : 2025-10-09 / Frt
#
fun_parse_file_ext <- function (file.name){
  
  # parse file name
  
  file.extension <- strsplit(file.name, "[.]")[[1]][2]
    
  # return
    
  return(file.extension)
  
}