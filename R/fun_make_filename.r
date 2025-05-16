#' @title Constructs a full image file name as date_ut_file_name_ext
#'
#' @description Constructs a full image file name as date_ut_file_name_ext. 
#' The resulting file name is optimised for the download from jsoc.
#' 
#' @param date Character date vector containing YYYY-MM-DD.
#'
#' @param ut Character time vector containing HHMM.
#' 
#' @param file_name_ext Character vector containing file name particel and file
#' extension.
#' 
#' @return character vector containing resulting image filename.
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
fun_make_filename <- function(date, ut, file_name_ext){
  
  key.words <- strsplit(as.character(date), "-")[[1]]
  
  yr <- key.words[1]          
  mt <- key.words[2]      
  dy <- key.words[3]
  
  ymd = paste0(yr,mt,dy)
  
  time <- as.character(as.integer(ut*100))
  
  filename <- paste(ymd, time, file_name_ext, sep = "_")
  
  return(filename)
  
}