#' @title Implements mathematical image manipulations.
#'
#' @description Implements three mathematical image manipulations, including 
#'   multiplication, subtraction and summation.
#'
#' @param im1 tibble containing at least 3 columns with pixel coordinates i and j
#'   and pixel values of first image.
#'
#' @param values_1 name of variable in im1 containing the pixel values.
#' 
#' @param im2 tibble containing at least 3 columns with pixel coordinates i and j
#'   and pixel values of second image.
#'
#' @param values_2 name of variable in im2 containing the pixel values.
#' 
#' @param method calculation method. Implemented are three methods: "mult" for
#'   image multiplication, "diff" for image subtraction and "summ" for image 
#'   summation.
#'   
#' @param values.name name of variable containing the calculated pixel values.
#' 
#' @return a tibble with additional column containing the resulting image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-04 / Frt
# - `Created`    : 2019-11-24 / Frt
# - `Last test`  : 2019-12-15 / Frt
#
fun_disc_math <- function(im1, values_1 = "x", im2, values_2 = "fill", 
                          method = "mult", values.name = "image"){
  
  image1 <- cbind(im1 %>% select(i, j), im1[,values_1])
  names(image1) <- c("i","j","im1")
  
  image2 <- cbind(im2 %>% select(i, j), im2[,values_2])
  names(image2) <- c("i","j","im2")
  
  y <- image1 %>% 
    left_join(image2, by=c("i","j")) 
    
  if (method == "mult"){
    z <- y %>% 
      mutate(im = im1 * im2)
  }
  
  if (method == "diff"){
    z <- y %>% 
      mutate(im = im1 - im2)
  } 
  
  if (method == "summ"){
    z <- y %>% 
      mutate(im = im1 + im2)
  }
  
  im3 <-  cbind(im1, im2[,values_2])
  names(im3) <- c(names(im1), eval(values_2))
  
  im4 <-  im3 %>% 
    left_join(z, by=c("i","j")) %>% 
    mutate(values = if_else(is.na(im),0,im)) %>% 
    select(-im1, -im2, -im)
  
  names(im4) <- c(names(im3), eval(values.name)) 
  
  return(im4)
  
}
