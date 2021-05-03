#' @title Imports a single FITS file
#'
#' @description Imports single FITS file given the file path and name. Returns 
#'   tibble with image, tibble with hdrlst and character vector header. 
#'
#' @param inp_data_path string with input data path.
#'
#' @param inp_file_name string with input file name.
#'
#' @param sdo.image if TRUE the image is originally an imported sdo jpg-file.
#'   
#' @param cut.image if TRUE the image contains additional text in the corners.
#'   
#' @return list with fitsim, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-31 / Frt
# - `Created`    : 2019-12-22 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
mod_fits_import <- function(inp_data_path, inp_file_name, sdo.image = "FALSE",
                            cut.image = "FALSE"){
  
  # reads FITS frame and header 
  
  im <- fun_read_image(paste0(inp_data_path,inp_file_name))
  
  header <- im$header
  
  # parses tibble with header keyword values
  
  hdrlst <- fun_hdr2list(im$hdr) 
  
  # constructs sdo-specific keywords, if necessary
  
  sdo.keywords <- fun_sdo_keywords(file.name = inp_file_name,
                                   header = header, 
                                   hdrlst = hdrlst,
                                   sdo.image = sdo.image)
    
  header <- sdo.keywords$header
  hdrlst <- sdo.keywords$hdrlst
    
  # calculates ephemeris for physical coordinates of the Sun
  
  sun.ephem <- fun_sun_ephem(hdrlst = hdrlst, sdo.image = sdo.image) 

  # converts matrix containing FITS frame in a tibble 
  
  fitsim <- fun_mat2tibbl(im$imDat)
  
  # updates hdrlst and header
  
  hdrlst$FILENAME <- inp_file_name
  hdrlst$CUTIMAGE <- cut.image
  hdrlst$SOLAR_P0 <- sun.ephem$P0
  hdrlst$SOLAR_B0 <- sun.ephem$B0
  hdrlst$SOLAR_L0 <- sun.ephem$L0
  hdrlst$SOLAR_R  <- sun.ephem$SD
  hdrlst$CAR_ROT  <- sun.ephem$CAR_ROT
  
  cimages <- addKwv("FILENAME", inp_file_name, "Original input file name",
                    header)
  cimages <- addKwv("CUTIMAGE", cut.image, "Input file has text in the corners",
                    cimages)
  cimages <- addKwv("SOLAR_P0", sun.ephem$P0, "P0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_B0", sun.ephem$B0, "B0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_L0", sun.ephem$L0, "L0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_R", sun.ephem$SD, "Diameter of solar disc (arcsec)",
                    cimages)
  cimages <- addKwv("CAR_ROT", sun.ephem$CAR_ROT,
                    "Carrington solar rotation number",
                    cimages)
  header <- cimages
  
  # flips the image, if required
  
  im_flip <- fun_flip_image(image = fitsim, 
                            hdrlst = hdrlst,
                            header = header,
                            sdo.image = sdo.image)
  
  fitsim <- im_flip$flipped_image
  hdrlst <- im_flip$hdrlst
  header <- im_flip$header
  
  # updates header
  
  cimages <- addHistory("  FITS file import with sunviewr::mod_fits_import",
                        header)
  
  header <- cimages
  
  # return
  
  z <- list(fitsim = fitsim, hdrlst = hdrlst, header = header)
  
  return(z)
  
}