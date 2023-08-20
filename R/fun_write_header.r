#' @title Writes image header
#'
#' @description Writes and closes the image header.
#'
#' @param x image data matrix.
#'
#' @param type implemented are "single" and "byte" for 16 bit and 8 bit images.
#'
#' @param bscale pixel_values = bzero + bscale * FITS_value.
#'
#' @param bzero pixel_values = bzero + bscale * FITS_value.
#'
#' @param header string vector with card images of the existing header and of
#'   new or modified card images.
#'
#' @return z tibble with additional column to x containing the flattened image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-08-19 / Frt
# - `Created`    : 2019-12-10 / Frt
# - `Last test`  : 2019-12-26 / Frt
#
fun_write_header <- function (x, type = "single", bscale = 1, bzero = 0, 
                              header = ""){
    
  naxisn <- dim(x)
  naxis  <- length(naxisn)
  
  type   <- tolower(substr(type, 1, 1))

  switch(type, b = {
    bitpix <- 8
  }, s = {
    bitpix <- 16
  }, stop("Unrecognized data type in 
          sunxplrr:fun_write_header: not byte or single"))

  cimages <- sprintf("%-80s", 
    "SIMPLE  = T                      / File conforms to FITS standard")
    
  cimages <- addKwv("BITPIX", bitpix, "number of bits per data pixel", 
                      cimages)
  cimages <- addKwv("NAXIS", naxis, "number of data axes", 
                      cimages)
  
  tmp <- character(naxis)
    
  for (i in 1:naxis) {
    
    tmp[i] <- newKwv(sprintf("NAXIS%d", i), naxisn[i], "length of data axis")
    
  }
  cimages <- c(cimages, tmp)
  
  cimages <- addKwv("BSCALE", bscale, "overall scaling", cimages)
  cimages <- addKwv("BZERO", bzero, "overall offset", cimages)
  
  cimages <- addComment("  Written by the R language sunxplrr package", 
                        cimages)
  cimages <- addComment("  FITS (Flexible Image Transport System) format is defined in", 
                        cimages)
  cimages <- addComment("  Astronomy and Astrophysics, volume 376, page 359 (2001)", 
                        cimages)
 
  if (length(header) > 0) {
    
    reserved <- c("^ *SIMPLE ", "^ *BITPIX ", "^ *NAXIS", "^ *BSCALE ", 
                  "^ *BZERO ", "^ *END ", 
                  "^ *COMMENT   Written by the R language sunxplrr package", 
                  "^ *COMMENT   FITS \\(Flexible Image Transport System\\) format is defined in", 
                  "^ *COMMENT   Astronomy and Astrophysics, volume 376, page 359 (2001)")
    
    for (string in reserved) {
      idx <- grep(string, header, ignore.case = TRUE)
      if (length(idx) > 0) 
        header <- header[-idx]
    }
  
    cimages <- c(cimages, header)
  
  }
  
  closeHdr(cimages)

}