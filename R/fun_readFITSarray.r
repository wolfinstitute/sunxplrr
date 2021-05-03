#' @title Reads FITS binary array from disc
#'
#' @description Reads 16 bit or 8 bit FITS binary array from disc. 
#'
#' @param zz binary port to file.
#'
#' @param hdr parsed header keyword and values vector.
#'
#' @return list with image matrix and parsed header vector.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-26 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2019-12-26 / Frt
#
fun_readFITSarray <- function (zz, hdr){
  naxis <- as.numeric(hdr[which(hdr == "NAXIS") + 1])
  switch(hdr[which(hdr == "BITPIX") + 1], `16` = {
    bsize <- 2
    btype <- integer()
    bsign <- TRUE
  }, `8` = {
    bsize <- 1
    btype <- integer()
    bsign <- FALSE
  }, stop("Unknown BITPIX request in readFITSarray"))
  NAXISn <- integer(naxis)
  numwords <- 1
  for (i in 1:naxis) {
    tmp <- as.numeric(hdr[which(hdr == paste("NAXIS", i, 
                                             sep = "")) + 1])
    numwords <- numwords * tmp
    NAXISn[i] <- tmp
  }
  D <- array(readBin(zz, what = btype, n = numwords, size = bsize, 
                     signed = bsign, endian = "big"), dim = NAXISn)
  tmp <- hdr[which(hdr == "BSCALE") + 1]
  BSCALE <- ifelse(length(tmp) != 1, 1, as.numeric(tmp))
  tmp <- hdr[which(hdr == "BZERO") + 1]
  BZERO <- ifelse(length(tmp) != 1, 0, as.numeric(strsplit(tmp, " ")[[1]][1]))
  if (BSCALE != 1 || BZERO != 0) 
    D <- D * BSCALE + BZERO
  list(imDat = D, hdr = hdr)
}
