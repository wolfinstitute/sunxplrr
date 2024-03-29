% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_mat2tibbl.r
\name{fun_mat2tibbl}
\alias{fun_mat2tibbl}
\title{Transforms a matrix to a tibble or a data.frame}
\usage{
fun_mat2tibbl(x, tibbl = TRUE)
}
\arguments{
\item{x}{matrix with i columns and j rows.}

\item{tibbl}{boolian If TRUE, the a tibble will be returned, otherwise a
data.frame.}
}
\value{
a tibble or a data.frame containing 3 columns wih matrix indices
i and j and values x.
}
\description{
Transforms a matrix to a tibble or a data.frame.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
