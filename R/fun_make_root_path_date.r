#' @title Constructs an extended image root path with root_path part and
#' daily subfolder structure.
#'
#' @description Constructs an extended image root path with root_path part and
#' daily subfolder structure. The resulting path is optimised for the download
#' of image files from jsoc.
#' 
#' @param date Character date vector containing YYYY-MM-DD.
#'
#' @param root_path Character vector containing root path.
#' 
#' @return character vector containing resulting extended root path.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2025-05-16 / Frt
# - `Created`    : 2025-05-16 / Frt
# - `Last test`  : 2025-05-16 / Frt
#
fun_make_root_path_date <- function(date, root_path){
  
  key.words <- strsplit(as.character(date), "-")[[1]]
  
  yr <- key.words[1]          
  mt <- key.words[2]      
  dy <- key.words[3]
  
  path_ext <- paste(yr, mt, dy, sep = "/")
  
  path <- paste0(root_path, path_ext, sep = "/")
  
  return(path)
  
}