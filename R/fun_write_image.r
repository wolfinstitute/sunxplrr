#' @title Writes FITS image to disc
#'
#' @description Writes 16 bit or 8 bit FITS image to disc. 
#'
#' @param x image data matrix.
#'
#' @param file output path and file name.
#'
#' @param hdrlst tibble with header keywords and values.
#'
#' @param header string vector with card images of the existing header and of
#'   new or modified card images.
#'
#' @return nothing.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-12-10 / Frt
# - `Last test`  : 2019-12-26 / Frt
#
fun_write_image <- function(x, file, hdrlst, header){
    
  if (hdrlst$BITPIX != "8" & hdrlst$BITPIX != "16"){
    
    stop(paste0("Bitdepth ", hdrlst$BITPIX, " is not implemented"))
    
  }
  
  if (hdrlst$BITPIX == "8"){
    
    if (max(x, na.rm = TRUE) <= (2^8 - 1)){
      bscale <- 1
    } else {
      bscale <- (2^8 -1)/(max(x, na.rm = TRUE))
    }
    x      <- x * bscale
    bzero  <- 0
    type <- "byte"
    
  }
  
  if (hdrlst$BITPIX == "16"){
    
    if (max(x, na.rm = TRUE) <= (2^16 - 1)){
      bscale <- 1
    } else {
      bscale <- (2^16 -1)/(max(x, na.rm = TRUE))
    }
    x      <- x * bscale
    bzero  <- 2^15
    x      <- x - bzero
    type <- "single"
    
  }
  
  x <- array(as.integer(x), dim = dim(x))
  
  # construct header
    
  hdr0 <- fun_write_header(x, type = type, bscale = bscale, bzero = bzero, 
                           header = header)
  
  # write to file
    
  y <- file(file, "wb")
    
  writeChar(hdr0, y, eos = NULL)
    
  type <- tolower(substr(type, 1, 1))
    
  if (is.integer(x)) {
      
    switch(type, b = {
      size <- 1
    }, s = {
      size <- 2
    }, stop("Unrecognized data type: not byte or single"))
    
  } else {
    
    switch(type, s = {
      size <- 4
    }, stop("Unrecognized data type: not single"))
    
  }
    
  writeBin(as.vector(x), y, size = size, endian = "big")
    
  pad <- raw(2880 - (length(as.vector(x)) * size)%%2880)
  writeBin(pad, y, endian = "big")
    
  close(y)
    
  return(invisible(hdr0))
  
}