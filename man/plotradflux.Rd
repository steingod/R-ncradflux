\name{plotradflux}

\alias{plotradflux}

\title{
    Creates at twin plot of shortwave and longwave broadband irradiance
}

\description{
    Creates a standardised time series plot of radiative fluxes. Upper
    panel shows shortwave irradiance (global radiation) and lower plot
    downward longwave radiation.
}

\usage{
    plotradflux(x)
}

\arguments{
    \item{x}{radflux object as returned by \link{readradflux}}
}

\value{
}

\seealso{
    \link{readradflux},\link{longwave_downward}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    plotradflux(radflux_bi)
}

\keyword{
}
