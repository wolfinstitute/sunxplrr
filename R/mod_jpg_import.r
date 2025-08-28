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

# - `Last change`: 2025-05-20 / Frt
# - `Created`    : 2025-05-20 / Frt
# - `Last test`  : 2025-05-20 / Frt
#
mod_jpg_import <- function(inp_data_path,
                           inp_file_name, 
                           gamma = 1.5,
                           sdo.image = "FALSE", 
                           flip.image = "FALSE",
                           flop.image = "FALSE"){
  
  # Reads JPG image 
  im <- fun_read_jpg_image(paste0(inp_data_path,inp_file_name))
  
  header <- im$header
  hdrlst <- fun_hdr2list(im$hdr) 
  
  # Converts image matrix to tibble
  fitsim <- fun_mat2tibbl(im$imDat)
  
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
    mutate(x = as.integer(x * (2^as.numeric(hdrlst$BITPIX) - 1)))
  
  # updates header
  
  cimages <- addHistory("  FITS file import with sunxplrr::mod_jpg_import",
                        header)
  
  header <- cimages
  
  # return
  
  z <- list(fitsim = fitsim, hdrlst = hdrlst, header = header)
  
  return(z)
  
}