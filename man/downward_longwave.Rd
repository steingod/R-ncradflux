\name{downward_longwave}

\alias{downward_longwave}

\title{
    Estimate the downward longwave radiation
}

\description{
    Kipp and Zonen CGR4 pyrgeometers measures a radiative balance. To get
    the atmospheric downward longwave radiation, an estimate using the
    sensor temperature of the instrument is required. This implementation
    follows the specification within the Kipp and Zonen CGR4 reference
    manual.
}

\usage{
    downward_longwave(x)
}

\arguments{
    \item{x}{An radflux object as returned by \link{readradflux}}
}

\value{
    A radflux object with longwave balance replaced by downward
    irradiance.
}

\seealso{
    \link{readradflux}, \link{plotradflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- downward_longwave(tmp)
}

\keyword{
}
