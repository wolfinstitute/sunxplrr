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

# - `Last change`: 2025-04-02 / Frt
# - `Created`    : 2019-12-26 / Frt
# - `Last test`  : 2025-04-02 / Frt
#
fun_sdo_keywords <- function (file.name, header, hdrlst, 
                          telescope = "SDO/AIA", 
                          object = "Solar photosphere at temperature minimum",
                          instrument = "AIA_3",
                          waveunit = "Angstrom",
                          sdo.image = "FALSE"){
  
  # dummy use for non sdo calcium images allowed
  
  if (sdo.image){
    
  # parses file name
  
  key.words <- fun_parse_filename(file.name)
  
  # provides keywords
  
  date.obs    <- key.words$date.obs
  naxisn      <- key.words$naxisn       # not used as already provided
  wavelnth    <- key.words$wavelnth                     

  # updates hdrlst
  
  hdrlst$TELESCOP   <- telescope
  hdrlst$OBJECT     <- object
  hdrlst$INSTRUME   <- instrument
  hdrlst$WAVELNTH   <- wavelnth
  hdrlst$WAVEUNIT   <- waveunit
  hdrlst$`DATE-OBS` <- date.obs
  
  # updates header
  
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