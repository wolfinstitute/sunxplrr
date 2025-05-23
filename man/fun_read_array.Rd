% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_read_array.r
\name{fun_read_array}
\alias{fun_read_array}
\title{Reads FITS binary array from disc}
\usage{
fun_read_array(zz, hdr)
}
\arguments{
\item{zz}{binary port to file.}

\item{hdr}{parsed header keyword and values vector.}
}
\value{
list with image matrix and parsed header vector.
}
\description{
Reads 16 bit or 8 bit FITS binary array from disc.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
