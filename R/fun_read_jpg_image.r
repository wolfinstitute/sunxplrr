#' @title Reads JPG image from disc
#'
#' @description Reads monochrome or three color JPG image from disc. Constructs
#'  minimal FITS header, corrects gamma and returns image with the provided 
#'  bit depth.
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

# - `Last change`: 2025-04-02 / Frt
# - `Created`    : 2025-03-16 / Frt
# - `Last test`  : 2025-03-17 / Frt
#
fun_read_jpg_image <- function(filename = "sunxplrr.jpg", 
                               gamma = 1.5, bitpix = 16){
  
  # Imports image as matrix
  imjpg <- jpeg::readJPEG(filename)
  
  # Chooses green channel if more than one color dimension is present
  if (length(dim(imjpg)) > 2){
    imjpg <- imjpg[,,2]
  }
  
  # Rotates image about -90 degrees
  fitsim <- fun_mat2tibbl(imjpg) |> 
    select(i=j, j=i, x)
  
  ymax <- max(fitsim$j) + 1
  
  fitsim <- fitsim |>  
    mutate(yj = as.integer(ymax - j)) |>   
    select(-j, yj, i, x) |> 
    select(i, j=yj, x)
  
  # Corrects gamma
  fitsim <- fitsim |>
    mutate(x = x^(1/gamma))
  
  # Scales image intensity values
  fitsim <- fitsim |> 
    mutate(x = as.integer(x * (2^bitpix - 1)))
 
  
   
  hdr0 <- fun_minimal_header(x = fun_tibbl2mat(fitsim), 
                             bitpix = bitpix, bscale = bscale, 
                             bzero = bzero, header = "")
  # 
  # 
  # 
  #   header <- fun_read_header(con, maxLines = maxLines)
  #   hdr <- fun_parse_header(headerName = header)
  #   D <- fun_readFITSarray(con, hdr)
  #   
  # 
  # 
  # D$header <- header
  
  # return(D)
}
