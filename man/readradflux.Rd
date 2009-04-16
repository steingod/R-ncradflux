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
    readradflux(file,readUVMED=FALSE,readSSITEMP=TRUE,readDLITEMP=TRUE)
}

\arguments{
    \item{file}{Filename and path to read}
    \item{readUVMED}{Boolean indicating whether to read UV med/hour if
    this is available}
    \item{readSSITEMP}{Boolean indicating whether to read the sensor
    temperature of the CMP21 if this is available}
    \item{readDLITEMP}{Boolean indicating whether to read the sensor
    temperature of the CGR4 if this is available}
}

\value{
    A \link{list} containing some metadata and the observations.
}

\seealso{
    \link{longwave_downward},\link{plotradflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- readradflux("/disk1/data/radflux-bergen-2008.nc")
}

\keyword{
}
