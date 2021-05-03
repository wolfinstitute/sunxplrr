#' @title Updates the header and hdrlst with sdo-specific keywords
#'
#' @description Reconstructs the FITS keywords DATE-OBS, NAXISn and WAVELNTH 
#'   from the provided sdo file name. Updates the header and hdrlst with some 
#'   other missing keywords.
#'
#' @param filename original file name of sdo image.
#'
#' @param header list containing image FITS header.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param sdo.image boolean switch for dummy use in the case of non sdo calcium
#'   images.
#'
#' @return list with updated header and hdrlst.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-27 / Frt
# - `Created`    : 2019-12-26 / Frt
# - `Last test`  : 2019-12-27 / Frt
#
fun_sdo_keywords <- function (file.name, header, hdrlst, sdo.image = "FALSE"){
  
  # dummy use for non sdo calcium images allowed
  
  if (sdo.image){
    
  # parse file name
  
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
  
  naxisn      <- key.words[3]  # not used as already provided
  wavelnth    <- key.words[4]                     

  # missing keywords for sdo calcium images
  
  telescop <- "SDO/AIA"
  object   <- "Solar photosphere at temperature minimum"
  instrume <- "AIA_3"
  waveunit <- "Angstrom"  # this is conforming to FITS standard
  
  # update hdrlst
  
  hdrlst$TELESCOP   <- telescop
  hdrlst$OBJECT     <- object
  hdrlst$INSTRUME   <- instrume
  hdrlst$WAVELNTH   <- wavelnth
  hdrlst$WAVEUNIT   <- waveunit
  hdrlst$`DATE-OBS` <- date.obs
  
  # update header
  
  cimages <- addKwv("TELESCOP", telescop, "Telescope",
                    header)
  cimages <- addKwv("OBJECT", object, "Object",
                    cimages)
  cimages <- addKwv("INSTRUME", instrume, "Instrument",
                    cimages)
  cimages <- addKwv("WAVELNTH", wavelnth, "Wavelength",
                    cimages)
  cimages <- addKwv("WAVEUNIT", waveunit, "Unit of Wavelength",
                    cimages)
  cimages <- addKwv("DATE-OBS", date.obs, "Date and time of observation in UT",
                    cimages)
  
  header <- cimages
  
  }
  
  # return
  
  z <- list(header = header, hdrlst = hdrlst)
  
  return(z)
  
}