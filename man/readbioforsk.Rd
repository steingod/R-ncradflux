\name{readbioforsk}

\alias{readbioforsk}

\title{
    Read Bioforsk data
}

\description{
    Reads ASCII files containing data from Bioforsk stations that include
    global radiation and solar insolation. These data are reformatted from
    the original raw data stream into a ASCII format at METNO.

    These files are of two slightly different variants (details to be
    included).
}

\usage{
    readbioforsk(file)
}

\arguments{
    \item{file}{the name and path to the file to be read}
}

\value{
    \link{list} containing station id, station name and a
    \link{data.frame} with the actual data.
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
