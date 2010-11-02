#
# $Id: toaflux.R,v 1.3 2010-11-02 08:53:37 steingod Exp $
#
# Estimates the TOA broadband irradiance.
#

toaflux <- function(tst, lat, lon, s0=1370) {

    doy <- as.numeric(strftime(as.POSIXlt(tst),"%j"))

    toa <- s0*(esd(doy))*deg2rad((solzen(tst,lat,lon)))

    return(toa)
}
