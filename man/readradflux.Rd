\name{readradflux}

\alias{readradflux}

\title{
    Read radiative fluxes from NetCDF/CF file
}

\description{
    Reads NetCDF/CF files conforming to the DAMOCLES specification (see
    \url{http://damocles.met.no/data_management/format.html})
}

\usage{
    readradflux(file)
}

\arguments{
    \item{file}{Filename and path to read}
}

\value{
    A \link{list} containing some metadata and the observations.
}

\seealso{
    \link{longwave_downrad},\link{plotradflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- readradflux("/disk1/data/radflux-bergen-2008.nc")
}

\keyword{
}
