#' @title Parses file names for mandatory keyword DATE-OBS
#'
#' @description Reconstructs mandatory FITS keywords from the provided file
#'  name.
#'
#' @param file.name string original image file name.
#'
#' @param parse.method string instrument specific parsing method. Implemented 
#'  are the SDO/HMI naming convention of Intensity based files from JSOC.
#'
#' @return list with parsed date.obs.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-07 / Frt
# - `Created`    : 2025-04-02 / Frt
# - `Last test`  : 2025-10-07 / Frt
#
fun_parse_filename <- function (file.name, parse.method ="SDO/HMI"){
  
  # parse file name type SDO
  
  if (parse.method == "SDO/HMI"){
  
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
  
  z <- list(date.obs = date.obs)
  
  return(z)
  
}