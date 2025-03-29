#' @title Reads FITS image from disc
#'
#' @description Reads 16 bit or 8 bit FITS image from disc. 
#'
#' @param filename input path and file name.
#'
#' @param maxLines maximal number of header lines to search for END statement.
#'   May be increased for very large headers.
#'
#' @return list with image matrix, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#' 
#' @export

# - `Last change`: 2025-03-29 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2025-03-29 / Frt
#
fun_read_image <- function(filename = "sunxplrr.fits", maxLines = 5000){
  
  con <- file(filename, "rb")
  
    header <- fun_read_header(con, maxLines = maxLines)
    hdr <- fun_parse_header(header)
    im <- fun_read_array(con, hdr)
    
  close(con)
  
  im$header <- header
  
  return(im)
}
