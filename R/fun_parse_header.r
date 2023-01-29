#' @title Parses FITS header
#'
#' @description Parses FITS header. COMMENT, HISTORY, empty and some preserved 
#'   keywords are omitted.
#'
#' @param headerName list containing image FITS header.
#'
#' @return Character vector with parsed FITS header entries.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-31 / Frt
# - `Created`    : 2019-12-25 / Frt
# - `Last test`  : 2019-12-31 / Frt
#
fun_parse_header <- function (headerName){

  # remove comments from numeric value entries
  idx <- setdiff(1:length(headerName), grep("'", headerName))
  for (i in idx) {
    headerName[i] <- strsplit(headerName[i], "/")[[1]][1]
  }
  
  # split header in keywords and values
  idx <- 1:length(headerName)
  hdr <- unlist(strsplit(headerName[idx], "="))
  
  # remove comments from character value entries 
  idx <- grep("'", hdr)
  for (i in idx) {
    hdr[i] <- strsplit(hdr[i], "'")[[1]][2]
  }
  
  # trim spaces
  for (i in 1:length(hdr)) {
    hdr[i] <- sub("^ *", "", hdr[i])
    hdr[i] <- sub(" *$", "", hdr[i])
  }
  
  # remove unwanted keywords
  reserved <- c("^ *COMMENT ", "^ *HISTORY ", 
                "the image", "^$")
  for (string in reserved) {
    idx <- grep(string, hdr, ignore.case = TRUE)
    if (length(idx) > 0)
      hdr <- hdr[-idx]
  }
  
  # return
  hdr
}
