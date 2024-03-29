% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_disc_math.r
\name{fun_disc_math}
\alias{fun_disc_math}
\title{Implements mathematical image manipulations.}
\usage{
fun_disc_math(
  im1,
  values_1 = "x",
  im2,
  values_2 = "fill",
  method = "mult",
  values.name = "image"
)
}
\arguments{
\item{im1}{tibble containing at least 3 columns with pixel coordinates i and j
and pixel values of first image.}

\item{values_1}{name of variable in im1 containing the pixel values.}

\item{im2}{tibble containing at least 3 columns with pixel coordinates i and j
and pixel values of second image.}

\item{values_2}{name of variable in im2 containing the pixel values.}

\item{method}{calculation method. Implemented are three methods: "mult" for
image multiplication, "diff" for image subtraction and "summ" for image
summation.}

\item{values.name}{name of variable containing the calculated pixel values.}
}
\value{
a tibble with additional column containing the resulting image.
}
\description{
Implements three mathematical image manipulations, including
multiplication, subtraction and summation.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
