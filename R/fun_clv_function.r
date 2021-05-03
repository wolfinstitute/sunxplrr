#' @title Calculates the standardized center-to-limb-variation function
#'
#' @description Calculates the center-to-limb-variation function according to
#'   formulae published by different authors, including Allen in Cox (2000), 
#'   Pierce & Slaughter (1977) and Neckel & Labs (1994). Furthermore, a customly
#'   fitted clv may be calculated. Note, that this is the only available option 
#'   for SDO images, since near the temperature minimum, a near flat clv is 
#'   expected but the SDO images show a clear limb darkening.  
#'
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x, pixel coordinates xi, yj and angle theta from an image disc.
#'
#' @param clv.i0 central intensity of the fitted clv function of the image.
#'   
#' @param method implemented are "Allen", "PS", "NL" and "fit".
#'   
#' @param sdo.image boolean switch for dummy use in the case of non sdo calcium
#'   images.
#'   
#' @param clv.coeff vector with 6 coefficients from the fitted clv function.
#'   
#' @return z tibble with additional column to x containing the standardized clv
#'         function and the cf vector with coefficients of the standardized clv
#'         function.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2020-01-09 / Frt
# - `Created`    : 2019-11-25 / Frt
# - `Last test`  : 2020-01-09 / Frt
#
fun_clv_function <- function(x, clv.i0, sclv.method = "NL", sdo.image = "FALSE", 
                             clv.coeff = NULL){
  
  if (sdo.image){
    
    sclv.method <- "fit"
  
  }
  
  if (sclv.method == "Allen"){
    
    u <- +0.91 # for lambda = 0.39 micro meter, according to Cox (2000), p.357
    v <- -0.05 # for lambda = 0.39 micro meter, according to Cox (2000), p.357
    
    cf <- c(u,v)
    names(cf) <- c("u", "v")
    
    y <- x %>% 
      filter(fill > 0) %>% 
      mutate(theta = theta * pi /180) %>% 
      mutate(sclv = 1 - (u*(1 - cos(theta))) - 
             v*(1 - cos(theta)*cos(theta))) %>% 
      select(i, j, sclv)
    
    z <-  x %>% 
      left_join(y, by=c("i","j")) %>% 
      mutate(sclv = if_else(is.na(sclv),0,sclv * clv.i0))
  
  }
  
  if (sclv.method == "PS"){
  
    # Coefficients interpolated to 3933.74 nm by Thomas K. Friedli from
    # Pierce, K. and Slaughter, C.D., Solar Physics 51 (1977) 25 - 41

    a <-  0.151234116	
    b <-  0.802837841
    c <-  0.26232042	
    d <- -0.309652479	
    e <-  0.04649481
    f <-  0.046766171

    cf <- c(a,b,c,d,e,f)
    names(cf) <- c("intercept", "theta", "theta2", "theta3", "theta4", "theta5")
    
    y <- x %>% 
      filter(fill > 0) %>%
      mutate(theta = theta * pi /180) %>% 
      mutate(sclv = a + b*cos(theta) + c*(cos(theta)^2) +  d*(cos(theta)^3) + 
                    e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      select(i, j, sclv)
      
    z <-  x %>% 
      left_join(y, by=c("i","j")) %>% 
      mutate(sclv = if_else(is.na(sclv),0,sclv * clv.i0))
    
  }

  if (sclv.method == "NL"){
    
    # Coefficients interpolated to 3933.74 nm by Thomas K. Friedli from
    # Neckel, H. and Labs, D., Solar Physics 153 (1994) 91 - 114
    
    a <-  0.128455248
    b <-  0.955755484	
    c <- -0.156643512	
    d <-  0.251019821
    e <- -0.296121875
    f <-  0.117534833
    
    cf <- c(a,b,c,d,e,f)
    names(cf) <- c("intercept", "theta", "theta2", "theta3", "theta4", "theta5")
    
    y <- x %>% 
      filter(fill > 0) %>%
      mutate(theta = theta * pi /180) %>% 
      mutate(sclv = a + b*cos(theta) + c*(cos(theta)^2) +  d*(cos(theta)^3) + 
                    e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      select(i, j, sclv)
    
    z <-  x %>% 
      left_join(y, by=c("i","j")) %>% 
      mutate(sclv = if_else(is.na(sclv),0,sclv * clv.i0))
    
  }
  
  if (sclv.method == "fit"){
    
    # Coefficients calculated by custom clv fitting
    
    a0 <- as.numeric(clv.coeff[1])
    b0 <- as.numeric(clv.coeff[2])
    c0 <- as.numeric(clv.coeff[3])
    d0 <- as.numeric(clv.coeff[4])
    e0 <- as.numeric(clv.coeff[5])
    f0 <- as.numeric(clv.coeff[6])
    
    cf_sum <- a0 + b0 + c0 + d0 + e0 + f0
    
    a <- as.numeric(clv.coeff[1]) / cf_sum
    b <- as.numeric(clv.coeff[2]) / cf_sum
    c <- as.numeric(clv.coeff[3]) / cf_sum
    d <- as.numeric(clv.coeff[4]) / cf_sum
    e <- as.numeric(clv.coeff[5]) / cf_sum
    f <- as.numeric(clv.coeff[6]) / cf_sum

    cf <- c(a,b,c,d,e,f)
    names(cf) <- c("intercept", "theta", "theta2", "theta3", "theta4", "theta5")
    
    y <- x %>% 
      filter(fill > 0) %>%
      mutate(theta = theta * pi /180) %>% 
      mutate(sclv = a + b*cos(theta) + c*(cos(theta)^2) +  d*(cos(theta)^3) + 
                    e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      select(i, j, sclv)
    
    z <-  x %>% 
      left_join(y, by=c("i","j")) %>% 
      mutate(sclv = if_else(is.na(sclv),0,sclv * clv.i0))
    
  }
  
  return(list(z = z, cf = cf))
  
}