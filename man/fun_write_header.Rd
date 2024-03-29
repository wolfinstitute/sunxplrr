% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_write_header.r
\name{fun_write_header}
\alias{fun_write_header}
\title{Writes image header}
\usage{
fun_write_header(x, type = "single", bscale = 1, bzero = 0, header = "")
}
\arguments{
\item{x}{image data matrix.}

\item{type}{implemented are "single" and "byte" for 16 bit and 8 bit images.}

\item{bscale}{pixel_values = bzero + bscale * FITS_value.}

\item{bzero}{pixel_values = bzero + bscale * FITS_value.}

\item{header}{string vector with card images of the existing header and of
new or modified card images.}
}
\value{
z tibble with additional column to x containing the flattened image.
}
\description{
Writes and closes the image header.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
