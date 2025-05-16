#' @title Constructs a full image path with root_path part and daily subfolder 
#' structure. The image file name is constructed as date_ut_file_name_ext
#'
#' @description Constructs a full image path with root_path part and daily 
#' subfolder structure. The image file name is constructed as 
#' date_ut_file_name_ext. The resulting path is optimised for the download of 
#' image files from jsoc.
#' 
#' @param date Character date vector containing YYYY-MM-DD.
#'
#' @param ut Character time vector containing HHMM.
#' 
#' @param root_path Character vector containing root path.
#' 
#' @param file_name_ext Character vector containing file name particel and file
#' extension.
#' 
#' @return character vector containing resulting full image path.
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
fun_make_path <- function(date, ut, root_path, file_name_ext){
  
  year = as.integer(lubridate::year(date)) 
  month = as.integer(lubridate::month(date)) 
  day = as.integer(lubridate::day(date))
  
  key.words <- strsplit(as.character(date), "-")[[1]]
  
  yr <- key.words[1]          
  mt <- key.words[2]      
  dy <- key.words[3]
  
  ymd = paste0(yr,mt,dy)
  
  time <- as.character(as.integer(ut*100))
  
  filename <- paste(ymd, time, file_name_ext, sep = "_")
  
  path_ext <- paste(yr, mt, dy, filename, sep = "/")
  
  path <- paste0(root_path, path_ext)
  
  return(path)
  
}