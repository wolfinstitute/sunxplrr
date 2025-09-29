#' @title Parses file names for mandatory keyword DATE-OBS
#'
#' @description Reconstructs the FITS keyword DATE-OBS from the provided file
#'  name.
#'
#' @param file.name string original image file name.
#'
#' @return list with parsed date.obs.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-09-29 / Frt
# - `Created`    : 2025-04-02 / Frt
# - `Last test`  : 2025-09-29 / Frt
#
fun_parse_filename <- function (file.name){
  
  # parse file name type SDO
  
  key.words <- strsplit(strsplit(file.name, "[.]")[[1]][1], "_")[[1]]
  
  yr <- stringr::str_sub(key.words[1],1,4)              
  mt <- stringr::str_sub(key.words[1],5,6)         
  dy <- stringr::str_sub(key.words[1],7,8)       
  
  date <- paste(yr,mt,dy, sep = "-")
  
  hr <- stringr::str_sub(key.words[2],1,2)              
  mn <- stringr::str_sub(key.words[2],3,4)         
  sc <- stringr::str_sub(key.words[2],5,6)
  
  time <- paste(hr,mn,sc, sep = ":")

  date.obs    <- paste(date,time, sep = "T")
  
  # return
  
  z <- list(date.obs = date.obs)
  
  return(z)
  
}