#' @title Constructs minimal image header
#'
#' @description Constructs character vector with minimal FITS header keywords. 
#'  Adds keywords as provided by card images of an existing header.
#'
#' @param x image data matrix.
#'
#' @param bitpix equals to 16 or 8 corresponding to 16 bit or 8 bit images.
#'
#' @param header string vector with card images of an existing header.
#'
#' @return character vector with FITS header keywords.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-06 / Frt
# - `Created`    : 2019-12-10 / Frt
# - `Last test`  : 2025-10-06 / Frt
#
fun_minimal_header <- function (x, bitpix = 16, header = ""){
  
  if (bitpix != "8" & bitpix != "16"){
    
    stop(paste0("Bitdepth ", bitpix, " is not implemented 
                in sunxplrr::fun_minimal_header"))
    
  }
  
  if (bitpix == "8"){
    
    if (max(x, na.rm = TRUE) <= (2^8 - 1)){
      bscale <- 1
    } else {
      bscale <- (2^8 -1)/(max(x, na.rm = TRUE))
    }
    bzero  <- 0

  }
  
  if (bitpix == "16"){
    
    if (max(x, na.rm = TRUE) <= (2^16 - 1)){
      bscale <- 1
    } else {
      bscale <- (2^16 -1)/(max(x, na.rm = TRUE))
    }
    bzero  <- 2^15

  }
  
  # Construct header
  
  naxisn <- dim(x)
  naxis  <- length(naxisn)
  
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
  
  cimages <- addComment("  FITS (Flexible Image Transport System) format is defined in 'Astronomy", 
                        cimages)
  cimages <- addComment("  and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H", 
                        cimages)
  
  cimages <- addHistory("  Modified by the R language sunxplrr package", 
                        cimages)
  header <- cimages
  
  
  # Add provided header and strip in it the already set reserved keywords
  
  if (header != "") {
    
    reserved <- c("^ *SIMPLE ", "^ *BITPIX ", "^ *NAXIS", "^ *BSCALE ", 
                  "^ *BZERO ",  
                  "^ *COMMENT   FITS \\(Flexible Image Transport System\\) format is defined in 'Astronomy", 
                  "^ *COMMENT   and Astrophysics', volume 376, page 359; bibcode: 2001A&A...376..359H",
                  "^ *END ")
    
    for (string in reserved) {
      idx <- grep(string, header, ignore.case = TRUE)
      if (length(idx) > 0) 
        header <- header[-idx]
    }
  
    cimages <- c(cimages, header)
  
  }
  
  closeHdr(cimages)

  # Return
  
  return(cimages)
}