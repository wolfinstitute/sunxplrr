#' @title Transforms a matrix to a tibble or a data.frame
#'
#' @description Transforms a matrix to a tibble or a data.frame.
#'
#' @param x matrix with i columns and j rows.
#'
#' @param tibbl boolian If TRUE, the a tibble will be returned, otherwise a 
#'   data.frame.
#'
#' @return a tibble or a data.frame containing 3 columns wih matrix indices 
#'   i and j and values x.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-11-15 / Frt
# - `Last test`  : 2019-11-17 / Frt
#
fun_mat2tibbl <- function(x, tibbl = TRUE){
  
  y <- as.data.frame(x)
  colnames(y) <- as.character(1:ncol(x))

  y <- y %>% 
    mutate(i = 1:nrow(y)) %>% 
    tidyr::gather(key = j, value = x, -i) %>% 
    mutate(j = as.integer(as.numeric(j)))

  if (tibbl){
    y <- tibble::as_tibble(y)
  }

  return(y)
  
}