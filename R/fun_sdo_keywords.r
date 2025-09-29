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
#' @param sdo.image boolean switch for dummy use in the case of non sdo images.
#'
#' @return list with updated header and hdrlst.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-09-29 / Frt
# - `Created`    : 2019-12-26 / Frt
# - `Last test`  : 2025-09-29 / Frt
#
fun_sdo_keywords <- function (file.name, header, hdrlst, 
                              sdo.image = "FALSE"){
  
  # dummy use for non sdo calcium images allowed
  
  if (sdo.image){
    
  # parses file name
  
  date.obs <- fun_parse_filename(file.name)$date.obs
  
  # updates hdrlst
  
  hdrlst$`DATE-OBS` <- date.obs
  
  # updates header
  
  cimages <- addKwv("DATE-OBS", date.obs, "Date and time of observation in UT",
                    header)
  
  header <- cimages
  
  }
  
  # return
  
  z <- list(header = header, hdrlst = hdrlst)
  
  return(z)
  
}