\name{evalradflux}

\alias{evalradflux}

\title{
    Generate evaluation plots and averages.
}

\description{
    Average radiative fluxes for the selected parameter and integration
    period are returned as a \link{data.frame} with the data including
    representative times and a grfaphical plot of the values is produced.
    Currently a pure mean is offered, in the future additional methods
    should be offered. 
}

\usage{
    evalradflux(x,parameter="SSI",integration="DAILY",subet)
}

\arguments{
    \item{x}{An radflux object as returned by \link{readradflux}}
    \item{parameter}{The parameter to examine, valid values are "SSI" and
    "DLI"}
    \item{integration}{The integration period, valid values are "DAILY"
    (hourly), "DAILYCONT" (minute based) and "MONTHLY"}
    \item{subset}{Temporal subset. Not implemented yet!}
}

\value{
    A \link{data.frame} containing monthly mean of the selected parameter
    and the accompaning time.

}

\seealso{
    \link{readradflux}, \link{plotradflux}, \link{longwave_downward}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- evalradflux(tmp)
}

\keyword{
}
