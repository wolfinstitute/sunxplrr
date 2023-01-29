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
#' @param model implemented are "poly" for polynomial fit with 6 coefficients,
#'   "poly_with_plane" for polynomial fit with 6 coefficients and an additional
#'   2 parameter plane through the disc center and a third model named as
#'   "poly_with_plane_and_nonparametric_background" for polynomial fit with 
#'   6 coefficients, an additional 2 parameter plane and a nonparametric surface
#'   fit in a gam structure.
#'   
#' @param run indicates how many iterations for spot and faculae elimination 
#'   should be run. No more than 3 iterations are implemented.
#' 
#' @param clip.resid.out if "TRUE" the clipped residuals from the last fit 
#'   iteration are returned. In batch mode this output is not needed.
#' 
#' @param sclv.method implemented are "Allen", "PS", "NL" and "fit".
#'   
#' @param sdo.image boolean switch for dummy use in the case of non sdo calcium
#'   images.
#'
#' @param light.save if TRUE only a small selection of csv files are saved.
#'
#' @return tibble containing clv function, calib image and flat image.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-15 / Frt
# - `Created`    : 2019-12-13 / Frt
# - `Last test`  : 2020-01-15 / Frt
#
mod_clv_correction <- function(x, 
                               hdrlst, 
                               header, 
                               model = "poly_with_plane",
                               run = 3, 
                               clip.resid.out = "FALSE",
                               sclv.method = "NL",
                               sdo.image = "FALSE",
                               light.save = "FALSE"){

  # estimate clv function 
  
  clv.fit <- fun_clv_fitting(x, 
                             hdrlst = hdrlst, 
                             model = model, 
                             run = run, 
                             clip.resid.out = clip.resid.out,
                             light.save = light.save)
  
  disc.clv    <- clv.fit$z
  clv.coeff   <- clv.fit$clv.coeff
  fit.sigma   <- clv.fit$fit.sigma
  fit.clip.at <- clv.fit$fit.clip.at
  
  sum.clv.i0  <- sum(clv.coeff)
  
  # standardise clv function
  
  if (sdo.image){
   
    disc.sclv       <- disc.clv
    disc.sclv.coeff <- clv.coeff
    sclv.method <- "fit"
  
  } else {
    
    sclv.fun <-  fun_clv_function(disc.clv, 
                                clv.i0 = sum.clv.i0,
                                sclv.method = sclv.method, 
                                sdo.image = sdo.image,  
                                clv.coeff = clv.coeff)
  
    disc.sclv       <- sclv.fun$z
    disc.sclv.coeff <- sclv.fun$cf
  
  }
  
  if (sclv.method == "fit"){
    sclv.fit <- "TRUE"
  } else {
    sclv.fit <- "FALSE"
  }
  
  # calibrate image
  
  disc.calib <- fun_clv_calibrate(disc.sclv, sdo.image = sdo.image)
  
  # flatten and standardize the image
  
  if (sdo.image){
  
    disc.flat <- fun_clv_correct(disc.calib, name.clv = "clv")
    
  } else {
  
    disc.flat <- fun_clv_correct(disc.calib, name.clv = "sclv")
  
  }
  
  # update hdrlst and header
  
  hdrlst$SUMCLVI0  <- sum.clv.i0
  hdrlst$FITSIGMA  <- fit.sigma
  hdrlst$FITCLPAT  <- fit.clip.at
  hdrlst$SCLVFIT   <- sclv.fit
  
  cimages <- addKwv("SUMCLVI0", sum.clv.i0, "mean central clv intensity (ADU)",
                    header)
  cimages <- addKwv("FITSIGMA", fit.sigma, "stddev of fit residuals (ADU)",
                    cimages)
  cimages <- addKwv("FITCLPAT", fit.clip.at, 
                    "threshold for discarding residuals (ADU)",
                    cimages)
  cimages <- addKwv("SCLVFIT", sclv.fit, 
                    "fitted clv is used as standardised clv",
                    cimages)
  cimages <- addHistory("  CLV correction with sunviewr::mod_clv_correction",
                        cimages)

  header <- cimages
  
  # return
  
  z <- list(disc.flat = disc.flat, hdrlst = hdrlst, header = header)
  
  return(z)
  
}