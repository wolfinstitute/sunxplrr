#' @title Reads FITS ASCII header from disc
#'
#' @description Reads FITS ASCII header from disc.
#'
#' @param zz binary port to file.
#'
#' @param maxLines maximal number of header lines to search for END statement.
#'   May be increased for very large headers.
#'
#' @return Character vector with FITS header card images.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-08-19 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
fun_read_header <- function (zz, maxLines = 5000){
  num <- 36
  cols <- 80
  maxHdrs <- maxLines/num
  header <- "dummy start line"
  image <- character(num)
  start <- seq(1, 2880, by = 80)
  for (i in 1:maxHdrs) {
    inpString <- readChar(zz, 2880)
    if (nchar(inpString) != 2880) {
        txt <- paste("*** Header problem:", nchar(inpString), 
                     "characters instead of 2880 *** \n")
        close(zz)
        stop(txt)
    }
    for (j in 1:num) {
      image[j] <- substr(inpString, start[j], start[j] + 79)
    }
    idx <- grep("^ *END +", image, ignore.case = TRUE)
    if (length(idx) > 0) {
      image <- image[-(idx:num)]
      header <- c(header, image)
      header <- header[-1]
      header <- header[header != "                                                                                "]
      return(header)
    } else {
      header <- c(header, image)
    }
  }
  stop("Haven't found END in header after ", maxLines, " header lines in
       sunxplrr::fun_read_header")
}
