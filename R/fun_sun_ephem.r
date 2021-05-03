#' @title Calculates ephemeris for physical coordinates of the Sun
#'
#' @description Calculates ephemeris for physical coordinates of the Sun, 
#'   including P0, B0, L0, the Sun's apparent diameter in arcsecs and the 
#'   Carrington rotation number. 
#'   Formulae from: Meeus,J., Astronomical Algorithms, Willmann-Bell, 1991.
#' 
#' @param hdrlst list containing image FITS header keywords and values.
#'
#' @param sdo.image boolean switch for case of sdo image.
#'
#' @return tibble with P0, B0, L0, the Sun's apparent diameter in arcsecs and
#'   the Carrington rotation number.
#'
#' @author [Thomas K. Friedli](mailto:thomas.friedli@bluewin.ch)
#'
#' @export

# - `Last change`: 2019-12-28 / Frt
# - `Created`    : 2019-12-20 / Frt
# - `Last test`  : 2019-12-28 / Frt
#
fun_sun_ephem <- function(hdrlst, sdo.image = "FALSE"){
  
  date_time <- hdrlst$`DATE-OBS`
  
  # delta T = TD - UT in days.
  delta_t <- fun_deltaT(date_time) / 86400
  
  # Julian ephemeris date
  jde <- fun_date2jd(date_time) + delta_t

  # time measured in Julian centuries of 36525 ephemeris days from J2000.0
  T <- (jde - 2451545.0) / 36525
  
  # geometric mean longitude of the Sun referred to the mean equinox of the date
  L <- (280.46645 + 36000.76983 * T + 0.0003032 * T * T) %% 360
  
  # mean anomaly of the Sun
  MA <- (357.52910 + 35999.05030 * T - 0.0001559 * T * T - 
         0.00000048 * T * T * T) %% 360
  
  # eccentricity of the Earth's orbit
  e <-  0.016708617 - 0.000042037 * T - 0.0000001236 * T * T
  
  # Sun's equation of center
  C <- 	(1.914600 - 0.004817 * T - 0.000014 * T * T) * sin(MA * pi / 180) + 
        (0.019993 - 0.000101 * T) * sin(2 * MA * pi / 180) +
        (0.000290) * sin(3 * MA * pi / 180)
  
  # Sun's true longitude
  WL <- L + C
  
  # Sun's true anomaly
  v <- MA + C
  
  #Sun's radius vector expressed in astronomical units
  R = (1.000001018 * (1 - (e * e))) / (1 + e * cos(v * pi / 180))
  
  # Sun's apparent longitude referred to the true equinox of the date
  SL <- WL - 0.0056916 / R
  
  # ascending node of the Moon's mean orbit on the ecliptic
  Omega <- (125.04452 - 1934.136261 * T) %% 360
  
  # mean longitude of the Sun  
  LS <- (280.4665 + 36000.7698 * T) %% 360
  
  # mean longitude of the Moon
  LM <- (218.3165 + 481267.8813 * T) %% 360
  
  # nutation in longitude
  Delta_psi <-  (- 17.20 / 3600) * sin(Omega * pi / 180) - 
                   (1.32 / 3600) * sin((2 * LS) * pi / 180) -
                   (0.23 / 3600) * sin((2 * LM) * pi / 180) + 
                   (0.21 / 3600) * sin((2 * Omega) * pi / 180)
  
  # nutation in obliquity
  Delta_epsilon <- (9.20 / 3600) * cos(Omega * pi / 180) +
                   (0.57 / 3600) * cos(2 * LS * pi / 180) +
                   (0.10 / 3600) * cos(2 * LM * pi / 180) -
                   (0.09 / 3600) * cos(2 * Omega * pi / 180)
  
  # obliquity of Earth's orbit
  Epsilon <- 23.439291111 + (-46.8150 / 3600) * T + (-0.00059 / 3600) * T * T + 
    (0.001813 / 3600) * T * T * T + Delta_epsilon
  
  # Sun's apparent longitude with effect of nutation
  Lambda <- SL + Delta_psi
  
  # longitude of the central meridian on the Sun according Carrington
  Theta <- (jde - 2398220) * (360 / 25.38)
  
  # inclination of solar equator n the ecliptic
  I <- 7.25
  
  # longitude of the ascending node of the solar equator on the ecliptic 
  K <- 73.6667 + 1.3958333 * (jde - 2396758) / 36525
  
  x <- atan(- cos(Lambda * pi / 180) * tan(Epsilon * pi / 180)) * (180 / pi)
  
  # x has to be taken between -90 and +90
  if (x > 90) {
    x <- (x - 180) 
  }
  if (x < - 90) {
    x <- (x + 180) 
  }
  
  y <- atan(- cos((SL - K) * pi / 180) * tan(I * pi / 180)) * (180 / pi)
  
  # y has to be taken between -90 and +90
  if (y > 90) {
    y <- (y - 180) 
  }
  if (y < - 90) {
    y <- (y + 180) 
  }
  
  # Position angle of the Sun's northern axes
  P0 <- x + y
  
  # heliografic latitude of the center of the solar disk
  B0 <- asin(sin((SL - K) * pi / 180) * sin(I * pi / 180)) * (180 / pi)
  
  Eta <- atan2(-sin((SL - K) * pi / 180) * cos(I * pi / 180),
               -cos((SL - K) * pi / 180)) * (180 / pi)
  
  # heliografic longitude of the central meridian
  L0 <- (Eta - Theta) %% 360 
  
  # diameter of solar disc in arcsec
  SD <- (2 * 959.63) / R
  
  # Carrington rotation number
  C0 <- (jde - 2398140.2270) / 27.2752316
  
  M0 <- 281.96 + 26.882476 * C0
  
  corr <- 0.1454 * sin(M0 * pi / 180) - 
          0.0085 * sin((2 * M0) * pi / 180) - 
          0.0141 * cos((2 * M0) * pi / 180)
  
  CAR_ROT <- floor((jde + corr - delta_t - 2398140.2270) / 27.2752316)

  # return

  if (sdo.image){
    P0 <- 0
  }
  
  z <- tibble(P0 = P0, B0 = B0, L0 = L0, SD = SD, CAR_ROT = CAR_ROT)

  return(z)

}