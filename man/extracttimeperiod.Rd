\name{extracttimeperiod}

\alias{extracttimeperiod}

\title{
    Subsets a time series
}

\description{
    Extracts a subserie of a time series.
}

\usage{
    extracttimeperiod(x, start, end)
}

\arguments{
    \item{x}{a \link{readradflux} object}
    \item{start}{A \link{datetime} object}
    \item{end}{A \link{datetime} object}
}

\value{
    An object of same type (\link{list}) as returned by \link{readradflux}
}

\seealso{
    \link{readradflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    extracttimeperiod(radflux_bi)
}

\keyword{
}
