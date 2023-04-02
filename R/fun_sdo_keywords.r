#' @title Updates the header and hdrlst with instrument specific keywords
#'
#' @description Reconstructs the FITS keywords DATE-OBS and WAVELNTH from the 
#'   provided file name. Defaults for the FITS keywords TELESCOP, OBJECT, 
#'   INSTRUME and WAVEUNIT are set to sdo aia 3 images. Updates the header
#'   and hdrlst.
#'
#' @param file.name string original image file name.
#'
#' @param header list containing image FITS header.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param telescope string name of telescope.
#'
#' @param object string name of object.
#'
#' @param instrument string name of instrument or camera.
#'
#' @param waveunit string name of wave unit.
#'
#' @param sdo.image boolean switch for dummy use in the case of non sdo images.
#'
#' @return list with updated header and hdrlst.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-04-02 / Frt
# - `Created`    : 2019-12-26 / Frt
# - `Last test`  : 2019-12-27 / Frt
#
fun_sdo_keywords <- function (file.name, header, hdrlst, 
                          telescope = "SDO/AIA", 
                          object = "Solar photosphere at temperature minimum",
                          instrument = "AIA_3",
                          waveunit = "Angstrom",
                          sdo.image = "FALSE"){
  
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

  # update hdrlst
  
  hdrlst$TELESCOP   <- telescope
  hdrlst$OBJECT     <- object
  hdrlst$INSTRUME   <- instrument
  hdrlst$WAVELNTH   <- wavelnth
  hdrlst$WAVEUNIT   <- waveunit
  hdrlst$`DATE-OBS` <- date.obs
  
  # update header
  
  cimages <- addKwv("TELESCOP", telescope, "Telescope",
                    header)
  cimages <- addKwv("OBJECT", object, "Object",
                    cimages)
  cimages <- addKwv("INSTRUME", instrument, "Instrument",
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