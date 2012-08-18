\name{ plotdaygraph }

\alias{ plotdaygraph }

\title{
    Plot solar insolation at a station during the day
}

\description{
    This function plots the insolation at a station as provided through
    observations. Provided a time series, a "normal" day can be plotted
    using mean, max or similar functions on all the data provided.
}

\usage{
    plotdaygraph(x,timezone="UTC",interval=10,decimal=2,method=max,...)
}

\arguments{
    \item{x}{A data object as returned by \link{readradflux}}
    \item{timezone}{Specifcation of the timezone, valid values are UTC,
    CET and TST (for true solar time)}
    \item{interval}{Specification of aggregation interval in minutes if original
    values are not used (typically this is 10 or 15)}
    \item{decimal}{obsolete}
    \item{method}{Function to use}
    \item{...}{Additional plotting parameters}
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
