\name{radfluxest}

\alias{radfluxest}

\title{
    Internal functions
}

\description{
    Internal functions used to estimate or analyse radiative fluxes.
}

\usage{
    fm_deg2rad(degrees)
    fm_rad2deg(radians)
    fm_time2tst(x,tz,lon)
    fm_ha(tst)
    fm_solzen(tst,lat,lon)
    fm_bidirrefl(refl,date,sunz)
    fm_esd(doy)
    fm_dec(doy)
    fm_eqt(doy)
    fm_findsollum{sunz, satname, date}
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
    \code{fm_deg2rad} and \code{fm_rad2deg} are used for conversion of
    angles during calculations.

    \code{fm_time2tst} converts time from a time zone to a true solar time or
    local time at the actual position.

    \code{fm_ha} estimates the hour angle.

    \code{fm_solzen} estimates the solar zenith angle for a position and
    time.

    \code{fm_bidirrefl} converts from albedo to bi-directional
    reflectance.

    \code{fm_esd} estimates the correction factor for the distance between
    the Earth and the Sun.

    \code{fm_decl} estimates the solar declination.

    \code{fm_eqt} estimates the equation of time.

    \code{fm_findsollum} estimates the TOA radiance for a specific sensor
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
