#' @title Fits the clv function, calibrate it and flattens the image
#'
#' @description Fits a six parameter polynom to the limb-variation-function. 
#'   Additionnally, a plane may be fitted trough the center of the image disc. 
#'   Removes all fitted non clv variation from the image and flattens it to 
#'   intensity contrasts. Be aware, that the resulting intensity contrast values
#'   should be multiplied with some appropriate factor before converting them 
#'   back to FITS, since in the FITS image all values are treated as integers. 
#'
#' @param x tibble containing columns wih pixel coordinates i and j and all 
#'   the calculated image information as provided by the disc extraction module.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param mean.method implemented are "mean" and "median", with "mean" as 
#'   default.
#'   
#' @param sdo.image boolean switch for dummy use in the case of non SDO images.
#'
#' @return tibble containing clv function, calib image and flat image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-01 / Frt
# - `Created`    : 2019-12-13 / Frt
# - `Last test`  : 2025-10-01 / Frt
#
mod_disc_flat <- function(x, 
                          hdrlst, 
                          header, 
                          mean.method = "mean",
                          sdo.image = "FALSE"){

  # rescale sdo flat image
  
  if (sdo.image) {

    x.median <- x |> 
      filter(fill > 0) |> 
      summarise(median = median(image),
                mean = mean(image))
    
    if (mean.method == "mean"){
      
      x.flat <- x |> 
        filter(fill > 0) |> 
        mutate(flat = image / x.median$mean) |> 
        select(i,j,flat)
      
    } else {
      
      x.flat <- x |> 
        filter(fill > 0) |> 
        mutate(flat = image / x.median$median) |> 
        select(i,j,flat)
      
    }
    
    disc.flat <-  x %>% 
      left_join(x.flat, by=c("i","j")) %>% 
      mutate(flat = if_else(is.na(flat),0,flat)) 
    
  } else {
    
    disc.flat <- x
    
  }
  
  # update hdrlst and header
  
  hdrlst$IMMEDIAN  <- x.median$median
  hdrlst$IMMEAN    <- x.median$mean
 
  cimages <- addKwv("IMMEDIAN", x.median$median, "median intensity value (ADU)",
                    header)
  cimages <- addKwv("IMMEAN", x.median$mean, "mean intensity value (ADU)",
                    cimages)
  cimages <- addHistory("  CLV correction with sunviewr::mod_disc_flat",
                        cimages)

  header <- cimages
  
  # return
  
  z <- list(disc.flat = disc.flat, hdrlst = hdrlst, header = header)
  
  return(z)
  
}