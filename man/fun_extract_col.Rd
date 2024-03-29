% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_extract_col.r
\name{fun_extract_col}
\alias{fun_extract_col}
\title{Extracts a single column of image frame}
\usage{
fun_extract_col(x, col = NULL)
}
\arguments{
\item{x}{tibble containing 3 columns with pixel coordinates i and j and pixel
intensity values x.}

\item{col}{int index of column to be extracted.}
}
\value{
tibble containing 2 columns with row numbers j and pixel
intensity values x.
}
\description{
Extracts a single column of image frame.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
