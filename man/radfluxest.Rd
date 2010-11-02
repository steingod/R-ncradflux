\name{radfluxest}

\alias{radfluxest}

\title{
    Internal functions
}

\description{
    Internal functions used to estimate or analyse radiative fluxes.
}

\usage{
    deg2rad(degrees)
    rad2deg(radians)
    time2tst(x,tz,lon)
    ha(tst)
    solzen(tst,lat,lon)
    bidirrefl(refl,date,sunz)
    esd(doy)
    dec(doy)
    eqt(doy)
    findsollum{sunz, satname, date}
}

\arguments{
    \item{degrees}{angle provided in degrees}
    \item{radians}{angle provided in radians}
    \item{tst}{True Solar Time or local time as Epoch}
    \item{lat}{geographical latitude, positive northwards}
    \item{lon}{geographical longitude, positive eastwards}
    \item{sunz}{solar zenith angle in degrees}
    \item{doy}{day of year, provided in the range 1-366}
    \item{satname}{Textual identification of a satellite sensor and band}
    \item{date}{time as Epoch}
}

\details{
    \code{deg2rad} and \code{rad2deg} are used for conversion of
    angles during calculations.

    \code{time2tst} converts time from a time zone to a true solar time or
    local time at the actual position.

    \code{ha} estimates the hour angle.

    \code{solzen} estimates the solar zenith angle for a position and
    time.

    \code{bidirrefl} converts from albedo to bi-directional
    reflectance.

    \code{esd} estimates the correction factor for the distance between
    the Earth and the Sun.

    \code{decl} estimates the solar declination.

    \code{eqt} estimates the equation of time.

    \code{findsollum} estimates the TOA radiance for a specific sensor
    band. \bold{It is currently not fully finished!!}
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
