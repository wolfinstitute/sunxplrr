#' @title Extracts image header to list
#'
#' @description Extracts the image header to a list of keywords with values.
#'  COMMENT and HISTORY lines are omitted.
#'
#' @param hdr FITS hdr
#'
#' @return list containing FITS header keywords and corresponding values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-08-20 / Frt
# - `Created`    : 2019-11-22 / Frt
# - `Last test`  : 2023-08-20 / Frt
#
fun_hdr2list = function(hdr) { 
  
  reserved <- c("^ *COMMENT ", 
                "^ *HISTORY ")
  
  for (string in reserved) {
    idx <- grep(string, hdr, ignore.case = TRUE)
    if (length(idx) > 0) 
      hdr <- hdr[-idx]
  }
  
  reserved2 <- c("^ *Astronomy")
  
  for (string in reserved2) {
    idx <- grep(string, hdr, ignore.case = TRUE)
    if (length(idx) > 0) 
      hdr<- hdr[-c(idx,idx+1)]
  }
  
  myL = list() 
  i = 1 
  while (i < length(hdr)) { 
    myL[[hdr[i]]] = hdr[i+1] 
    i = i + 2 
  }
  
  return(myL)
  
} 