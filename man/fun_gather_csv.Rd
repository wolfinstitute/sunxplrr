% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_gather_csv.r
\name{fun_gather_csv}
\alias{fun_gather_csv}
\title{Gathers output files of an index}
\usage{
fun_gather_csv(out_data_path, index.name = "_total_indices")
}
\arguments{
\item{out_data_path}{full path to output directory.}

\item{index.name}{character string with full index name as contained in the
file name after the image id and before the file extension.}
}
\value{
tibble containing the calibrated image values.
}
\description{
Gathers all output files of an index within an out_data_path.
}
\author{
\href{mailto:thomas.k.friedli@bluewin.ch}{Thomas K. Friedli}
}
