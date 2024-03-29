% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RCflux.R
\name{standardiseNames}
\alias{standardiseNames}
\title{Standardises names in a data frame}
\usage{
standardiseNames(df)
}
\arguments{
\item{df}{A data frame.}
}
\description{
This function standardises names in a data frame.
}
\examples{
tdf <- data.frame(Plot = c(1, 2), Compound.Name = c("CO2", "CH4"))
tdf
names(tdf)
tdf <- standardiseNames(tdf)
tdf
names(tdf)
}
\seealso{
\code{\link{convert_GC_output}} for the higher-level function which calls this.
}
\keyword{GC}
\keyword{input}
