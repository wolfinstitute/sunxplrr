#' @title Extracts image header to list
#'
#' @description Extracts the image header to a list of keywords with values.
#'  COMMENT and HISTORY lines are omitted.
#'
#' @param h FITS hdr from FITSio::readFITS(image)
#'
#' @return list containing FITS header keywords and corresponding values.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-11-22 / Frt
# - `Last test`  : 2019-12-12 / Frt
#
fun_hdr2list = function(h) { 
  
  reserved <- c("^ *COMMENT ", 
                "^ *HISTORY ")
  
  for (string in reserved) {
    idx <- grep(string, h, ignore.case = TRUE)
    if (length(idx) > 0) 
      h <- h[-idx]
  }
  
  myL = list() 
  i = 1 
  while (i < length(h)) { 
    myL[[h[i]]] = h[i+1] 
    i = i + 2 
  }
  
  return(myL)
  
} 