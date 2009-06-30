\name{averadflux}

\alias{averadflux}

\title{
    Average radiative fluxes.
}

\description{
    Average radiative fluxes and return a \link{data.frame} with the data
    including representative times. Currently a pure mean is offered, in
    the future additional methods should be offered. Beware that the
    interface of this function may change in the future.
}

\usage{
    averadflux(x)
}

\arguments{
    \item{x}{An radflux object as returned by \link{readradflux}}
}

\value{
    A \link{data.frame} containing monthly mean of DLI, SSI and the
    accompaning time.

}

\seealso{
    \link{readradflux}, \link{plotradflux}, \link{longwave_downward}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- averadflux(tmp)
}

\keyword{
}
