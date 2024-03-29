% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_clv_fitting.r
\name{fun_clv_fitting}
\alias{fun_clv_fitting}
\title{Fits an up to eight-parameter center-to-limb-variation function}
\usage{
fun_clv_fitting(
  x,
  hdrlst,
  model = "poly_with_plane",
  run = 3,
  clip.resid.out = "FALSE",
  light.save = "FALSE"
)
}
\arguments{
\item{x}{tibble containing columns with pixel coordinates i and j, pixel
values x from the original image, border and fill from the disc mask,
image from the extracted image disc, pixel coordinates xi and yj measured
from the estimated disc center and the heliografic coordinates theta, phi,
L and B.}

\item{hdrlst}{list containing image FITS header keywords and values.}

\item{model}{implemented are "poly" for polynomial fit with 6 coefficients,
"poly_with_plane" for polynomial fit with 6 coefficients and an additional
2 parameter plane through the disc center and a third model named as
"poly_with_plane_and_nonparametric_background" for polynomial fit with
6 coefficients, an additional 2 parameter plane and a nonparametric surface
fit in a gam structure.}

\item{run}{indicates how many iterations for spot and faculae elimination
should be run. No more than 3 iterations are implemented.}

\item{clip.resid.out}{if "TRUE" the clipped residuals from the fit iterations
are returned. In batch mode this output is not needed.}

\item{light.save}{if TRUE only a small selection of csv files are saved.}
}
\value{
list containing the tibble z with the fitted model, the clv function
and the residuals, two vectors clv.coeff and fit.coeff containing the
fitted coefficients of the fitted model and of the fitted clv function and
two values with the stdev of the fit and the clip value for low and high
signals, if applied.
}
\description{
Fits a six parameter polynomial to the clv function. On request,
a 2 parameter plane may be fitted trough the center of the image disc.
Dark sunspots and bright faculae may be eliminated by up to two elimination
runs, where with the first iteration run all 99\% outliers and with the
second iteration run all 95\% outliers are eliminated.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
