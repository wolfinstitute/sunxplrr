#' @title Recalibrates flat image
#'
#' @description Recalibrates the flat image to [0,1] interval. Be aware, that 
#'   the resulting intensity contrast values should be multiplied by some 
#'   appropriate factor before converting them back to FITS, since in the FITS 
#'   image all values are treated as integers. 
#'
#' @param x tibble containing columns wih pixel coordinates i and j and all 
#'   the calculated image information as provided by the disc extraction module.
#'
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param header list containing image FITS header.
#'
#' @param mean.method string implemented methods are "mean", "median" and 
#'   "mode", with "mode" as default.
#'   
#' @param sdo.image boolean switch for dummy use in the case of non SDO images.
#'
#' @return list containing rescaled flat image, hdrlst and header.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2025-10-07 / Frt
# - `Created`    : 2019-12-13 / Frt
# - `Last test`  : 2025-10-07 / Frt
#
mod_disc_flat <- function(x, 
                          hdrlst, 
                          header, 
                          mean.method = "mode",
                          sdo.image = "FALSE"){

  x <- discim
  
  # rescale sdo flat image
  
  if (sdo.image) {

    # full disc values implement switch
    
    switch(
      mean.method,
      mean = {
        
        x.means <- x |> 
          filter(fill > 0) |> 
          summarise(mean = mean(image))
        
        x.flat <- x |> 
          filter(fill > 0) |> 
          mutate(flat = image / x.means$mean) |> 
          select(i,j,flat)
        
        # update hdrlst and header
        
        hdrlst$IMMEAN    <- x.means$mean
        
        cimages <- addKwv("IMMEAN", x.means$mean, "mean intensity value (ADU)",
                          header)
        
        header <- cimages
        
      },
      median = {
        
        x.means <- x |> 
          filter(fill > 0) |> 
          summarise(median = median(image))
        
        x.flat <- x |> 
          filter(fill > 0) |> 
          mutate(flat = image / x.means$median) |> 
          select(i,j,flat)
        
        # update hdrlst and header
        
        hdrlst$IMMEDIAN  <- x.means$median
        
        cimages <- addKwv("IMMEDIAN", x.means$median, "median intensity value (ADU)",
                          header)
        
        header <- cimages
        
      },
      mode = {
        
        x.means <- x |> 
          filter(fill > 0) |> 
          summarise(mode = fun_calc_mode(image))
        
        x.flat <- x |> 
          filter(fill > 0) |> 
          mutate(flat = image / x.means$mode) |> 
          select(i,j,flat)
        
        # update hdrlst and header
        
        hdrlst$IMMODE  <- x.means$mode
        
        cimages <- addKwv("IMMODE", x.means$mode, "mode intensity value (ADU)",
                          header)
        
        header <- cimages
        
      }
    )
    
    disc.flat <-  x %>%
      left_join(x.flat, by=c("i","j")) %>%
      mutate(flat = if_else(is.na(flat),0,flat))
    
  } else {
    
    disc.flat <- x |> 
      mutate(flat = if_else(is.na(image),0,image))
    
  }
  
  # update hdrlst and header
  
  cimages <- addHistory("  CLV correction with sunviewr::mod_disc_flat",
                        header)

  header <- cimages
  
  # return
  
  z <- list(disc.flat = disc.flat, hdrlst = hdrlst, header = header)
  
  return(z)
  
}