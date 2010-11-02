\name{averadflux}

\alias{averadflux}

\title{
    Average radiative fluxes.
}

\description{
    Average radiative fluxes and return a \link{data.frame} with the data
    including representative times. Currently a pure mean is offered, in
    the future additional methods should be offered. Beware that the
    interface of this function may change in the future.

    In the current setup only estimation of the mean value for a period
    (month, day or hour) is supported using the function \link{mean}. The
    resulting estimate is representative for the following time. E.g.
    hourly values are valid for the hour after the time.
}

\usage{
    averadflux(x,period="month")
}

\arguments{
    \item{x}{An radflux object as returned by \link{readradflux}}
    \item{period}{Period to sum over. Valid keywords are \code{month}
    (default), \code{day} (day of year) and \code{hour}.
}

\value{
    A \link{data.frame} similar in structure to the one returned by
    \link{readradflux} containing the sum over the specified period of
    DLI, SSI and the accompaning time as well as the number of elements
    that are used to estimate each average value. Furthermore, the
    information element of the input object is attached in rewritten form.

}

\seealso{
    \link{readradflux}, \link{plotradflux}, \link{longwave_downward},
    \link{evalradflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- averadflux(tmp)
}

\keyword{
}
