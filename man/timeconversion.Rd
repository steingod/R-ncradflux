\name{timeconversion}

\alias{loncorr equationoftime utc2tst cet2tst}

\title{
    Utility functions for conversion between timezones
}

\description{
    Utility functions that are used to convert time specifications between
    timezones and true solar time. All functions are based on C-code taken
    from \code{libfmutil}.
}

\usage{
    loncorr(centralmeridian, longitude)
    equationoftime(doy)
    utc2tst(utc,lon)
    cet2tst(cet,lon)
}

\arguments{
    \item{centralmeridian}{Reference longitude for the correction factor}
    \item{longitude}{Actual longitude for the conversion}
    \item{utc}{\link{POSIXct} specification of UTC time}
    \item{cet}{\link{POSIXct} specification of Central European Time time}
}

\value{
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

