#' @title Extracts observation time from jsoc-type file names
#'
#' @description Extracts observation time from jsoc-type file names.
#'
#' @param file.name character string with full file name.
#'
#' @return character string with extracted observation time.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-05-16 / Frt
# - `Created`    : 2025-05-16 / Frt
# - `Last test`  : 2025-05-16 / Frt
#
fun_parse_obs_time <- function (file.name){
  
  # parse file name
  
  key.words <- strsplit(strsplit(file.name, "[.]")[[1]][1], "_")[[1]]
  
  obs_time <- key.words[2]    
  
  # return
  
  return(obs_time)
  
}