#' @title Updates the header and hdrlst with date and time information
#'
#' @description Reconstructs the FITS keyword DATE-OBS from the provided file 
#'   name. Updates the header and hdrlst.
#'
#' @param file.name string original image file name.
#'
#' @param header list containing image FITS header.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param parse.filename boolean switch for dummy use for frames with 
#'   existing fits keywords.
#' 
#' @param parse.method string method for parsing the provided file.name.
#'
#' @return list with updated header and hdrlst.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-09 / Frt
# - `Created`    : 2019-12-26 / Frt
# - `Last test`  : 2025-10-09 / Frt
#
fun_parse_keywords <- function (file.name, header, hdrlst, 
                                parse.filename = "FALSE", 
                                parse.method = "SDO/HMI"){
  
  # dummy use for frames with existing fits keywords is allowed
  
  if (parse.filename){
    
    # parses file name
    
    parsed.file.name <- fun_parse_filename(file.name, 
                                           parse.method = parse.method)
    
    date.obs <- parsed.file.name$date.obs
    
    # updates hdrlst
    
    hdrlst$`DATE-OBS` <- date.obs
    
    # updates header
    
    cimages <- addKwv("DATE-OBS", date.obs, "Date and time of observation in UT",
                      header)
    
    header <- cimages
    
    # return
    
    z <- list(header = header, hdrlst = hdrlst)
    
    return(z)
    
  }
  
  # return
  
  z <- list(header = header, hdrlst = hdrlst)
  
  return(z)
  
}