\name{ plotyeargraph }

\alias{ plotyeargraph }

\title{
    Plot solar insolation at a station during a year
}

\description{
    This function plots the insolation at a station as provided through
    observations. Provided a time series, a "normal" year can be plotted
    using mean, max or similar functions on all the data provided.
}

\usage{
    plotyeargraph(x,timezone="UTC",method=max,...)
}

\arguments{
    \item{x}{A data object as returned by \link{readradflux}}
    \item{timezone}{Specifcation of the timezone, valid values are UTC,
    CET and TST (for true solar time)}
    \item{method}{Function to use}
    \item{...}{Additional plotting parameters}
}

\value{
}

\seealso{
    \link{readradflux}, \link{plotyeargraph}, \link{extracttimeperiod}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
}

\keyword{
}
