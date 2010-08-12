\name{toaflux}

\alias{toaflux}

\title{
    Estimate TOA broadband irradiance
}

\description{
    Estimates the broadband irradiance at Top Of the Atmosphere (TOA)
    using a configurable Solar Constant. If no Solar Constant is provided,
    the standard value of 1370\note{To be checked!!}.
}

\usage{
    toaflux(tst, lat, lon, s0=1370)
}

\arguments{
    \item{tst}{True Solar Time or local time}
    \item{lat}{geographical latitude in degrees, positive northwards}
    \item{lon}{geographical longitude in degrees, positive eastwards}
    \item{s0}{Solar Constant, 1370W/m² unless changed}
}

\value{
    \link{numeric}
}

\seealso{
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}
