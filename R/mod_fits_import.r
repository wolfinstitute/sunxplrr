#' @title Imports a single FITS file
#'
#' @description Imports single FITS file given the file path and name. Returns 
#'   tibble with image, tibble with hdrlst and character vector header. Changes 
#'   image header, flips and flops the image, if requested.
#'
#' @param inp_data_path string with input data path.
#'
#' @param inp_file_name string with input file name.
#'
#' @param exchange.header boolean if TRUE the header of the original image will
#'   be changed.
#'   
#' @param inp_hdata_path string with input data path for new header file.
#'
#' @param inp_hfile_name string with input file name of the new header file.
#'
#' @param sdo.image boolean if TRUE the image is an imported sdo jpg-file.
#'   
#' @param flip.image boolean if TRUE the image will be flipped.
#'   
#' @param flop.image boolean if TRUE the image will be flopped.
#'   
#' @return list with fitsim, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-03-02 / Frt
# - `Created`    : 2019-12-22 / Frt
# - `Last test`  : 2025-03-02 / Frt
#
mod_fits_import <- function(inp_data_path,
                            inp_file_name, 
                            exchange.header = "FALSE", 
                            inp_hdata_path = inp_data_path,
                            inp_hfile_name = inp_file_name,
                            sdo.image = "FALSE", 
                            flip.image = "FALSE",
                            flop.image = "FALSE"){
  
  # reads FITS frame 
  
  im <- fun_read_image(paste0(inp_data_path,inp_file_name))
  
  # exchanges FITS header 
  
  im <- fun_exchange_header(im = im, 
                            exchange.header = exchange.header,
                            inp_hdata_path = inp_hdata_path,
                            inp_hfile_name = inp_hfile_name)
  
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
  
  # updates header
  
  cimages <- addHistory("  Modified by the R language sunxplrr package", 
                        header)
  header <- cimages
  
  # updates hdrlst and header
  
  hdrlst$FILENAME <- inp_file_name
  hdrlst$SOLAR_P0 <- sun.ephem$P0
  hdrlst$SOLAR_B0 <- sun.ephem$B0
  hdrlst$SOLAR_L0 <- sun.ephem$L0
  hdrlst$SOLAR_D  <- sun.ephem$SD
  hdrlst$CAR_ROT  <- sun.ephem$CAR_ROT
  
  cimages <- addKwv("FILENAME", inp_file_name, "Original input file name",
                    header)
  cimages <- addKwv("SOLAR_P0", sun.ephem$P0, "P0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_B0", sun.ephem$B0, "B0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_L0", sun.ephem$L0, "L0 angle (degrees)",
                    cimages)
  cimages <- addKwv("SOLAR_D", sun.ephem$SD, "Diameter of solar disc (arcsec)",
                    cimages)
  cimages <- addKwv("CAR_ROT", sun.ephem$CAR_ROT, "Carrington rotation number",
                    cimages)
  header <- cimages
  
  # flips the image, if required
  
  im_flip <- fun_flip_image(image = fitsim, 
                            hdrlst = hdrlst,
                            header = header,
                            flip.image = flip.image)
  
  fitsim <- im_flip$flipped_image
  hdrlst <- im_flip$hdrlst
  header <- im_flip$header
  
  # flops the image, if required
  
  im_flop <- fun_flop_image(image = fitsim, 
                            hdrlst = hdrlst,
                            header = header,
                            flop.image = flop.image)
  
  fitsim <- im_flop$flopped_image
  hdrlst <- im_flop$hdrlst
  header <- im_flop$header
  
  # updates header
  
  cimages <- addHistory("  FITS file import with sunxplrr::mod_fits_import",
                        header)
  
  header <- cimages
  
  # return
  
  z <- list(fitsim = fitsim, hdrlst = hdrlst, header = header)
  
  return(z)
  
}