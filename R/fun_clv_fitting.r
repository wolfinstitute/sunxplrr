#' @title Fits an up to eight-parameter center-to-limb-variation function
#'
#' @description Fits a six parameter polynomial to the clv function. On request,
#'   a 2 parameter plane may be fitted trough the center of the image disc. 
#'   Dark sunspots and bright faculae may be eliminated by up to two elimination 
#'   runs, where with the first iteration run all 99% outliers and with the 
#'   second iteration run all 95% outliers are eliminated.
#' 
#' @param x tibble containing columns with pixel coordinates i and j, pixel
#'   values x from the original image, border and fill from the disc mask, 
#'   image from the extracted image disc, pixel coordinates xi and yj measured
#'   from the estimated disc center and the heliografic coordinates theta, phi,
#'   L and B.
#'
#' @param hdrlst list containing image FITS header keywords and values.
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
#' @param clip.resid.out if "TRUE" the clipped residuals from the fit iterations 
#'   are returned. In batch mode this output is not needed.
#' 
#' @param light.save if TRUE only a small selection of csv files are saved.
#'
#' @return list containing the tibble z with the fitted model, the clv function
#'   and the residuals, two vectors clv.coeff and fit.coeff containing the
#'   fitted coefficients of the fitted model and of the fitted clv function and 
#'   two values with the stdev of the fit and the clip value for low and high 
#'   signals, if applied.
#'
#' @author [Thomas K. Friedli](mailto:thomas.k.friedli@bluewin.ch)
#'
#' @import dplyr
#'
#' @export

# - `Last change`: 2023-02-07 / Frt
# - `Created`    : 2019-11-25 / Frt
# - `Last test`  : 2020-01-15 / Frt
#
fun_clv_fitting <- function(x, hdrlst, model = "poly_with_plane", 
                            run = 3, clip.resid.out = "FALSE", 
                            light.save = "FALSE"){
  
  if (model == "poly"){
  
    if (run < 1 | run > 3) {
      stop("Number of runs has to be between 1 and 3")
    }
    
    y <- x %>%
      filter(.data$fill > 0) %>%
      mutate(theta = .data$theta * pi /180) %>% 
      mutate(theta1 = cos(.data$theta)) %>% 
      mutate(theta2 = cos(.data$theta)^2) %>% 
      mutate(theta3 = cos(.data$theta)^3) %>% 
      mutate(theta4 = cos(.data$theta)^4) %>% 
      mutate(theta5 = cos(.data$theta)^5)
    
    if (run > 0) {
      
      # first run
      
      fit1 <- lm(image ~ theta1 + theta2 + theta3 + theta4 + theta5, data = y)
      
      summry1 <- summary(fit1)
      resid1  <- residuals(fit1) 
      coeff1  <- coef(fit1)
      
      # analyse first run
      
      a <- as.numeric(coeff1[1])
      b <- as.numeric(coeff1[2])
      c <- as.numeric(coeff1[3])
      d <- as.numeric(coeff1[4])
      e <- as.numeric(coeff1[5])
      f <- as.numeric(coeff1[6])
      
      if (light.save){
        
       y1 <- cbind(y,resid1) %>% 
         mutate(clv = a + b*cos(.data$theta) + c*(cos(.data$theta)^2) +  
                      d*(cos(.data$theta)^3) + e*(cos(.data$theta)^4) + 
                      f*(cos(.data$theta)^5)) %>% 
         mutate(fit = a + b*cos(.data$theta) + c*(cos(.data$theta)^2) +  
                      d*(cos(.data$theta)^3) + e*(cos(.data$theta)^4) + 
                      f*(cos(.data$theta)^5)) %>% 
         mutate(resid = .data$image - .data$fit)
       
       z1 <- y1 %>% 
         select("i", "j", "clv", "resid", "resid1")
       
       if (clip.resid.out){
          
         z1 <-  x %>% 
           left_join(z1, by=c("i","j")) %>% 
           mutate(clv = if_else(is.na(.data$clv),0,.data$clv)) %>% 
           mutate(resid = if_else(is.na(.data$resid),0,.data$resid)) %>% 
           mutate(resid1 = if_else(is.na(.data$resid1),0,.data$resid1))
       
       } else {
        
         z1 <-  x %>% 
           left_join(z1, by=c("i","j")) %>% 
           mutate(clv = if_else(is.na(.data$clv),0,.data$clv)) %>% 
           mutate(resid = if_else(is.na(.data$resid),0,.data$resid)) %>% 
           select(-"resid1")
        
       }
      
      } else {
        
        y1 <- cbind(y,resid1) %>% 
          mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(fit = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(resid = image - fit)
        
        z1 <- y1 %>% 
          select(i, j, clv, fit, resid, resid1)
        
        if (clip.resid.out){
          
          z1 <-  x %>% 
            left_join(z1, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            mutate(resid1 = if_else(is.na(resid1),0,resid1))
          
        } else {
          
          z1 <-  x %>% 
            left_join(z1, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            select(-resid1)
          
        }
        
      }
      
      # return
      
      z     <- z1
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit1)
      fit.sigma <- summry1$sigma
      fit.clip.at <- NULL
      
    }
    
    if (run > 1) {
      
      # second run
      
      clip_at <- qt(0.995, df.residual(fit1))*summry1$sigma
      
      y.clipped <- cbind(y,resid1) %>% 
        mutate(clip = if_else(abs(resid1) > clip_at,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid1,-clip)
      
      fit2 <- lm(image ~ theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y.clipped)
      
      summry2 <- summary(fit2)
      resid2  <- residuals(fit2)
      coeff2  <- coef(fit2)
      
      # analyse second run
      
      a <- as.numeric(coeff2[1])
      b <- as.numeric(coeff2[2])
      c <- as.numeric(coeff2[3])
      d <- as.numeric(coeff2[4])
      e <- as.numeric(coeff2[5])
      f <- as.numeric(coeff2[6])
      
      y20 <- cbind(y.clipped,resid2)
      
      y21 <- y20 %>% 
        select(i, j, resid2)
      
      y22 <-  y %>% 
        left_join(y21, by=c("i","j")) %>% 
        mutate(resid2 = if_else(is.na(resid2),0,resid2))

      if (light.save){

      z21 <- y22 %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                     d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(resid = image - fit) %>% 
        select(i, j, clv, resid, resid2)

      if (clip.resid.out){
        
        z22 <-  x %>% 
          left_join(z21, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid2 = if_else(is.na(resid2),0,resid2))
      
      } else {
        
        z22 <-  x %>% 
          left_join(z21, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid2)
        
      }

      } else {

        z21 <- y22 %>% 
          mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(fit = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(resid = image - fit) %>% 
          select(i, j, clv, fit, resid, resid2)
        
        if (clip.resid.out){
          
          z22 <-  x %>% 
            left_join(z21, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            mutate(resid2 = if_else(is.na(resid2),0,resid2))
          
        } else {
          
          z22 <-  x %>% 
            left_join(z21, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            select(-resid2)
          
        }
      
      }      

      # return
      
      z     <- z22
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit2)
      fit.sigma <- summry2$sigma
      fit.clip.at <- clip_at
      
    }
    
    if (run > 2) {
      
      # third run
      
      clip_at2 <- qt(0.975, df.residual(fit2))*summry2$sigma
      
      y.clipped2 <- y20 %>% 
        mutate(clip = if_else(abs(resid2) > clip_at2,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid2,-clip)
      
      fit3 <- lm(image ~ theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y.clipped2)
      
      summry3 <- summary(fit3)
      resid3  <- residuals(fit3)
      coeff3  <- coef(fit3)
      
      # analyse third run
      
      a <- as.numeric(coeff3[1])
      b <- as.numeric(coeff3[2])
      c <- as.numeric(coeff3[3])
      d <- as.numeric(coeff3[4])
      e <- as.numeric(coeff3[5])
      f <- as.numeric(coeff3[6])
      
      y3 <- cbind(y.clipped2,resid3) %>% 
        select(i, j, resid3)
      
      y3 <-  y %>% 
        left_join(y3, by=c("i","j")) %>% 
        mutate(resid3 = if_else(is.na(resid3),0,resid3))
      
      if (light.save){
        
      z3 <- y3 %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                     d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(resid = image - fit) %>% 
        select(i, j, clv, resid, resid3)
      
      if (clip.resid.out){
        
        z3 <-  x %>% 
          left_join(z3, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid3 = if_else(is.na(resid3),0,resid3))
      
      } else {
        
        z3 <-  x %>% 
          left_join(z3, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid3)
        
      }
      
      } else {
        
        z3 <- y3 %>% 
          mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(fit = a + b*cos(theta) + c*(cos(theta)^2) +  
                   d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
          mutate(resid = image - fit) %>% 
          select(i, j, clv, fit, resid, resid3)
        
        if (clip.resid.out){
          
          z3 <-  x %>% 
            left_join(z3, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            mutate(resid3 = if_else(is.na(resid3),0,resid3))
          
        } else {
          
          z3 <-  x %>% 
            left_join(z3, by=c("i","j")) %>% 
            mutate(clv = if_else(is.na(clv),0,clv)) %>% 
            mutate(fit = if_else(is.na(fit),0,fit)) %>% 
            mutate(resid = if_else(is.na(resid),0,resid)) %>% 
            select(-resid3)
          
        }
        
      }
      
      # return
      
      z     <- z3
      clv.coeff   <- c(a,b,c,d,e,f)
      fit.coeff   <- coef(fit3)
      fit.sigma   <- summry3$sigma
      fit.clip.at <- clip_at2
      
    }
    
  }

  if (model == "poly_with_plane"){
    
    if (run < 1 | run > 3) {
      stop("Number of runs has to be between 1 and 3")
    }
    
    y <- x %>%
      filter(fill > 0) %>%
      mutate(theta = theta * pi /180) %>% 
      mutate(theta1 = cos(theta)) %>% 
      mutate(theta2 = cos(theta)^2) %>% 
      mutate(theta3 = cos(theta)^3) %>% 
      mutate(theta4 = cos(theta)^4) %>% 
      mutate(theta5 = cos(theta)^5)
    
    if (run > 0) {
    
      # first run
      
      fit1 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y)
      
      summry1 <- summary(fit1)
      resid1  <- residuals(fit1) 
      coeff1  <- coef(fit1)
      
      # analyse first run
      
      a <- as.numeric(coeff1[1])
      g <- as.numeric(coeff1[2])
      h <- as.numeric(coeff1[3])
      b <- as.numeric(coeff1[4])
      c <- as.numeric(coeff1[5])
      d <- as.numeric(coeff1[6])
      e <- as.numeric(coeff1[7])
      f <- as.numeric(coeff1[8])
      
      y1 <- cbind(y,resid1) %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>%  
        mutate(resid = image - fit)
        
      z1 <- y1 %>% 
        select(i, j, clv, fit, resid, resid1)
      
      if (clip.resid.out){
        
        z1 <-  x %>% 
          left_join(z1, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid1 = if_else(is.na(resid1),0,resid1))

      } else {
      
        z1 <-  x %>% 
          left_join(z1, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid1)
        
      }
      
      # return
      
      z     <- z1
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit1)
      fit.sigma <- summry1$sigma
      fit.clip.at <- NULL
      
    }
    
    if (run > 1) {
    
    # second run
    
    clip_at <- qt(0.995, df.residual(fit1))*summry1$sigma
      
    y.clipped <- cbind(y,resid1) %>% 
        mutate(clip = if_else(abs(resid1) > clip_at,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid1,-clip)
      
    fit2 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
               data = y.clipped)
    
    summry2 <- summary(fit2)
    resid2  <- residuals(fit2)
    coeff2  <- coef(fit2)
    
    # analyse second run
    
    a <- as.numeric(coeff2[1])
    g <- as.numeric(coeff2[2])
    h <- as.numeric(coeff2[3])
    b <- as.numeric(coeff2[4])
    c <- as.numeric(coeff2[5])
    d <- as.numeric(coeff2[6])
    e <- as.numeric(coeff2[7])
    f <- as.numeric(coeff2[8])
    
    y20 <- cbind(y.clipped,resid2)
    
    y21 <- y20 %>% 
      select(i, j, resid2)
    
    y22 <-  y %>% 
      left_join(y21, by=c("i","j")) %>% 
      mutate(resid2 = if_else(is.na(resid2),0,resid2))
    
    z21 <- y22 %>% 
      mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
               d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
               d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      mutate(resid = image - fit) %>% 
      select(i, j, clv, fit, resid, resid2)
    
    if (clip.resid.out){
      
      z22 <-  x %>% 
        left_join(z21, by=c("i","j")) %>% 
        mutate(clv = if_else(is.na(clv),0,clv)) %>% 
        mutate(fit = if_else(is.na(fit),0,fit)) %>% 
        mutate(resid = if_else(is.na(resid),0,resid)) %>% 
        mutate(resid2 = if_else(is.na(resid2),0,resid2))
    
    } else {
      
      z22 <-  x %>% 
        left_join(z21, by=c("i","j")) %>% 
        mutate(clv = if_else(is.na(clv),0,clv)) %>% 
        mutate(fit = if_else(is.na(fit),0,fit)) %>% 
        mutate(resid = if_else(is.na(resid),0,resid)) %>% 
        select(-resid2)
      
    }
    
    # return
    
    z     <- z22
    clv.coeff <- c(a,b,c,d,e,f)
    fit.coeff <- coef(fit2)
    fit.sigma <- summry2$sigma
    fit.clip.at <- clip_at
    
    }
    
    if (run > 2) {
    
    # third run
    
    clip_at2 <- qt(0.975, df.residual(fit2))*summry2$sigma
      
    y.clipped2 <- y20 %>% 
        mutate(clip = if_else(abs(resid2) > clip_at2,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid2,-clip)
      
    fit3 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
               data = y.clipped2)
    
    summry3 <- summary(fit3)
    resid3  <- residuals(fit3)
    coeff3  <- coef(fit3)
    
    # analyse third run
    
    a <- as.numeric(coeff3[1])
    g <- as.numeric(coeff3[2])
    h <- as.numeric(coeff3[3])
    b <- as.numeric(coeff3[4])
    c <- as.numeric(coeff3[5])
    d <- as.numeric(coeff3[6])
    e <- as.numeric(coeff3[7])
    f <- as.numeric(coeff3[8])
    
    y3 <- cbind(y.clipped2,resid3) %>% 
      select(i, j, resid3)
    
    y3 <-  y %>% 
      left_join(y3, by=c("i","j")) %>% 
      mutate(resid3 = if_else(is.na(resid3),0,resid3))
    
    z3 <- y3 %>% 
      mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
               d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
               d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
      mutate(resid = image - fit) %>% 
      select(i, j, clv, fit, resid, resid3)
    
    if (clip.resid.out){
      
      z3 <-  x %>% 
        left_join(z3, by=c("i","j")) %>% 
        mutate(clv = if_else(is.na(clv),0,clv)) %>% 
        mutate(fit = if_else(is.na(fit),0,fit)) %>% 
        mutate(resid = if_else(is.na(resid),0,resid)) %>% 
        mutate(resid3 = if_else(is.na(resid3),0,resid3))
    
    } else{
      
      z3 <-  x %>% 
        left_join(z3, by=c("i","j")) %>% 
        mutate(clv = if_else(is.na(clv),0,clv)) %>% 
        mutate(fit = if_else(is.na(fit),0,fit)) %>% 
        mutate(resid = if_else(is.na(resid),0,resid)) %>% 
        select(-resid3)

    }
    
    # return
    
    z     <- z3
    clv.coeff <- c(a,b,c,d,e,f)
    fit.coeff <- coef(fit3)
    fit.sigma <- summry3$sigma
    fit.clip.at <- clip_at2
    
    }
    
  }
  
  if (model == "poly_with_plane_and_nonparametric_background"){
  
    message("Nonparametric background fitting with gam not yet implemented")
    
    if (run < 1 | run > 3) {
      stop("Number of runs has to be between 1 and 3")
    }
    
    y <- x %>%
      filter(fill > 0) %>%
      mutate(theta = theta * pi /180) %>% 
      mutate(theta1 = cos(theta)) %>% 
      mutate(theta2 = cos(theta)^2) %>% 
      mutate(theta3 = cos(theta)^3) %>% 
      mutate(theta4 = cos(theta)^4) %>% 
      mutate(theta5 = cos(theta)^5)
    
    if (run > 0) {
      
      # first run
      
      fit1 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y)
      
      summry1 <- summary(fit1)
      resid1  <- residuals(fit1) 
      coeff1  <- coef(fit1)
      
      # analyse first run
      
      a <- as.numeric(coeff1[1])
      g <- as.numeric(coeff1[2])
      h <- as.numeric(coeff1[3])
      b <- as.numeric(coeff1[4])
      c <- as.numeric(coeff1[5])
      d <- as.numeric(coeff1[6])
      e <- as.numeric(coeff1[7])
      f <- as.numeric(coeff1[8])
      
      y1 <- cbind(y,resid1) %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>%  
        mutate(resid = image - fit)
      
      z1 <- y1 %>% 
        select(i, j, clv, fit, resid, resid1)
      
      if (clip.resid.out){
        
        z1 <-  x %>% 
          left_join(z1, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid1 = if_else(is.na(resid1),0,resid1))
        
      } else {
        
        z1 <-  x %>% 
          left_join(z1, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid1)
        
      }
      
      # return
      
      z     <- z1
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit1)
      fit.sigma <- summry1$sigma
      fit.clip.at <- NULL
      
    }
    
    if (run > 1) {
      
      # second run
      
      clip_at <- qt(0.995, df.residual(fit1))*summry1$sigma
      
      y.clipped <- cbind(y,resid1) %>% 
        mutate(clip = if_else(abs(resid1) > clip_at,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid1,-clip)
      
      fit2 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y.clipped)
      
      summry2 <- summary(fit2)
      resid2  <- residuals(fit2)
      coeff2  <- coef(fit2)
      
      # analyse second run
      
      a <- as.numeric(coeff2[1])
      g <- as.numeric(coeff2[2])
      h <- as.numeric(coeff2[3])
      b <- as.numeric(coeff2[4])
      c <- as.numeric(coeff2[5])
      d <- as.numeric(coeff2[6])
      e <- as.numeric(coeff2[7])
      f <- as.numeric(coeff2[8])
      
      y20 <- cbind(y.clipped,resid2)
      
      y21 <- y20 %>% 
        select(i, j, resid2)
      
      y22 <-  y %>% 
        left_join(y21, by=c("i","j")) %>% 
        mutate(resid2 = if_else(is.na(resid2),0,resid2))
      
      z21 <- y22 %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(resid = image - fit) %>% 
        select(i, j, clv, fit, resid, resid2)
      
      if (clip.resid.out){
        
        z22 <-  x %>% 
          left_join(z21, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid2 = if_else(is.na(resid2),0,resid2))
        
      } else {
        
        z22 <-  x %>% 
          left_join(z21, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid2)
        
      }
      
      # return
      
      z     <- z22
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit2)
      fit.sigma <- summry2$sigma
      fit.clip.at <- clip_at
      
    }
    
    if (run > 2) {
      
      # third run
      
      clip_at2 <- qt(0.975, df.residual(fit2))*summry2$sigma
      
      y.clipped2 <- y20 %>% 
        mutate(clip = if_else(abs(resid2) > clip_at2,0,1)) %>% 
        filter(clip > 0) %>% 
        select(-resid2,-clip)
      
      fit3 <- lm(image ~ xi + yj + theta1 + theta2 + theta3 + theta4 + theta5, 
                 data = y.clipped2)
      
      summry3 <- summary(fit3)
      resid3  <- residuals(fit3)
      coeff3  <- coef(fit3)
      
      # analyse third run
      
      a <- as.numeric(coeff3[1])
      g <- as.numeric(coeff3[2])
      h <- as.numeric(coeff3[3])
      b <- as.numeric(coeff3[4])
      c <- as.numeric(coeff3[5])
      d <- as.numeric(coeff3[6])
      e <- as.numeric(coeff3[7])
      f <- as.numeric(coeff3[8])
      
      y3 <- cbind(y.clipped2,resid3) %>% 
        select(i, j, resid3)
      
      y3 <-  y %>% 
        left_join(y3, by=c("i","j")) %>% 
        mutate(resid3 = if_else(is.na(resid3),0,resid3))
      
      z3 <- y3 %>% 
        mutate(clv = a + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(fit = a + g*xi + h*yj + b*cos(theta) + c*(cos(theta)^2) +  
                 d*(cos(theta)^3) + e*(cos(theta)^4) + f*(cos(theta)^5)) %>% 
        mutate(resid = image - fit) %>% 
        select(i, j, clv, fit, resid, resid3)
      
      if (clip.resid.out){
        
        z3 <-  x %>% 
          left_join(z3, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          mutate(resid3 = if_else(is.na(resid3),0,resid3))
        
      } else{
        
        z3 <-  x %>% 
          left_join(z3, by=c("i","j")) %>% 
          mutate(clv = if_else(is.na(clv),0,clv)) %>% 
          mutate(fit = if_else(is.na(fit),0,fit)) %>% 
          mutate(resid = if_else(is.na(resid),0,resid)) %>% 
          select(-resid3)
        
      }
      
      # return
      
      z     <- z3
      clv.coeff <- c(a,b,c,d,e,f)
      fit.coeff <- coef(fit3)
      fit.sigma <- summry3$sigma
      fit.clip.at <- clip_at2
      
    }
    
  }
  
  return(list(z = z, clv.coeff = clv.coeff, fit.coeff = fit.coeff, 
              fit.sigma = fit.sigma, fit.clip.at = fit.clip.at))
  
}