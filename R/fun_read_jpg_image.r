#' @title Reads JPG image from disc
#'
#' @description Reads monochrome or three color JPG image from disc. Constructs
#'  minimal FITS header and returns image with the provided bit depth.
#'
#' @param filename input path and file name.
#'
#' @param bitpix int image will be scaled to the provided bit depth.
#'
#' @return list with image matrix, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-05-21 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-05-21 / Frt
#
fun_read_jpg_image <- function(filename = "sun_logo.jpg", bitpix = 16){
  
  # Imports image as matrix
  imDat <- jpeg::readJPEG(filename)
  
  # Chooses green channel if more than one color dimension is present
  if (length(dim(imDat)) > 2){
    imDat <- imDat[,,2]
  }
  
  # Constructs minimal header
  header <- fun_minimal_header(x = imDat, bitpix = bitpix, header = "")
  
  # Strips unused information from header
  hdr <- fun_parse_header(header)
  
  # Return
  z <- list(imDat = imDat, hdr = hdr, header = header)
  
  return(z)
  
}
