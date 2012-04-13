\name{dumpresults}

\alias{dumpresults}

\title{
    Dump object to ASCII file
}

\description{
    Dumps radiative flux object to ASCII file. Currently only dump of
    objects returned by \link{averadflux} are supported.
}

\usage{
    dumpresults(path,x)
}

\arguments{
    \item{path}{path to where data are dumped, filenames are of the type
    radflux_location_date.txt}
    \item{x}{an object of the type returned by \link{averadflux}}
}

\value{
    NA
}

\seealso{
    \link{averadflux}
}

\author{
    Øystein Godøy (\email{o.godoy@met.no})
}

\examples{
    t <- readradflux("/disk1/data/radflux.nc")
    t1 <- averadflux(t,per="hour")
    dumpresults("myfile.txt",x)
}

\keyword{
}
