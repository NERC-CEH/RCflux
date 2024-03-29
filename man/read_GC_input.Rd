% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCflux.R
\name{read_GC_input}
\alias{read_GC_input}
\title{Reads a GC input file}
\usage{
read_GC_input(fname, dataSource = "aGC")
}
\arguments{
\item{fname}{A GC input file.}

\item{dataSource}{A character string identifying the source of the data: "aGC" from the Agilent GC, or "SRUC_GC" from the SRUC_GC.}
}
\description{
This function reads a GC input file.
}
\examples{
read_GC_input(fname)
}
\seealso{
\code{\link{convert_GC_output}} for the higher-level function which calls this.
}
\keyword{GC}
\keyword{input}
