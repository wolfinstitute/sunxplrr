#' @title Exchanges the header of a single FITS file
#'
#' @description Exchanges existing image header of a single FITS file with the
#'   header from another single FITS file. 
#'
#' @param im list with image matrix, parsed header and full header vectors.
#'
#' @param exchange.header boolean if TRUE the header of the original image will
#'   be changed.
#'   
#' @param inp_hdata_path string with input data path for new header file.
#'
#' @param inp_hfile_name string with input file name of the new header file.
#'
#' @return list with image matrix, exchanged parsed header and exchanged full
#'   header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-03-02 / Frt
# - `Created`    : 2023-08-18 / Frt
# - `Last test`  : 2025-03-02 / Frt
#
fun_exchange_header <- function(im, exchange.header = "FALSE",
                                inp_hdata_path,
                                inp_hfile_name){
  
  # exchange image header ------------------------------------------------------
  
  if (exchange.header){
  
    im_hfile <- fun_read_image(filename = paste0(inp_hdata_path,inp_hfile_name))
    
    im$imDat  <- im$imDat
    im$hdr    <- im_hfile$hdr
    im$header <- im_hfile$header
  
  }
  
  # return ---------------------------------------------------------------------
  
  return(im)
  
}