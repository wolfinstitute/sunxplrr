#' @title Reads JPG image
#'
#' @description Reads monochrome or three color JPG image. Gamma corrects, 
#'  rotates and scales image. Constructs minimal FITS header and returns 
#'  image with the provided bit depth.
#'
#' @param filename input path and file name.
#'
#' @param gamma gamma correction factor value.
#'
#' @param bitpix int image will be scaled to the provided bit depth.
#'
#' @return list with image matrix, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-10-07 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-10-07 / Frt
#
fun_read_jpg_image <- function(filename = "sun_logo.jpg", 
                               gamma = 1.5, bitpix = 16){
  
  # Imports image as matrix
  imDat <- jpeg::readJPEG(filename)
  
  # Chooses green channel if more than one color dimension is present
  if (length(dim(imDat)) > 2){
    imDat <- imDat[,,2]
  }

  # Converts image matrix to tibble
  fitsim <- fun_mat2tibbl(imDat)
  
  # Corrects gamma
  fitsim <- fitsim |>
    mutate(x = x^(1/gamma))
  
  # Rotates image about -90 degrees
  fitsim <- fitsim |> 
    select(i=j, j=i, x)
  
  ymax <- max(fitsim$j) + 1
  
  fitsim <- fitsim |>  
    mutate(yj = as.integer(ymax - j)) |>   
    select(-j, yj, i, x) |> 
    select(i, j=yj, x)
  
  # Scales image intensity values
  fitsim <- fitsim |> 
    mutate(x = as.integer(x * (2^bitpix - 1)))
  
  # Converts tibble to matrix
  imDat <- fun_tibbl2mat(x = fitsim)

  # Constructs minimal header
  header <- fun_minimal_header(x = imDat, bitpix = bitpix, add.header = "")
  
  # Strips unused information from header
  hdr <- fun_parse_header(header)
  
  # Return
  z <- list(imDat = imDat, hdr = hdr, header = header)
  
  return(z)
  
}
