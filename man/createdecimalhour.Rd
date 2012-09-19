\name{createdecimalhour}

\alias{createdecimalhour}

\title{
    Converts a time object to decimal hours
}

\description{
    Converts a time object to decimal hours which eases the comparison on
    daily basis with other data.
}

\usage{
    createdecimalhour(x, minuteinterval=-99, ...)
}

\arguments{
    \item{x}{a \link{DATETIMECLASSES} object}
    \item{minuteinterval}{Interval to round minute values to, could be
    e.g. 10 minutes, 15 minutes. Negative values cause no rounding to be
    performed}
}

\value{
    \link{numeric}
}

\seealso{
    \link{plotdaygraph}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    createdecimalhour(radflux_bi)
}

\keyword{
}
