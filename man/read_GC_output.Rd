% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCflux.R
\name{read_GC_output}
\alias{read_GC_output}
\title{This function reads Agilent GC output.}
\usage{
read_GC_output(fname, dataSource = "aGC")
}
\arguments{
\item{fname}{A GC output file from the Agilent GC.}

\item{dataSource}{A character string identifying the source of the data: "aGC" from the Agilent GC, or "SRUC_GC" from the SRUC_GC.}
}
\description{
Reads Agilent GC output
}
\seealso{
\code{\link{convert_GC_output}} for the higher-level function which calls this.
}
\keyword{Agilent}
\keyword{GC}
\keyword{output}
