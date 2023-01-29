#' @title Reads FITS image from disc
#'
#' @description Reads 16 bit or 8 bit FITS image from disc. 
#'
#' @param filename output path and file name.
#'
#' @param maxLines maximal number of header lines to search for END statement.
#'   May be increased for very large headers.
#'
#' @return list with image matrix, parsed header and full header vectors.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-26 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2019-12-26 / Frt
#
fun_read_image <- function(filename = "sunxplrr.fits", maxLines = 5000){
  zz <- file(filename, "rb")
  header <- fun_read_header(zz, maxLines = maxLines)
  hdr <- fun_parse_header(headerName = header)
  D <- fun_readFITSarray(zz, hdr)
  close(zz)
  D$header <- header
  return(D)
}
