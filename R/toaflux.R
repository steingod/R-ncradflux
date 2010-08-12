#
# $Id: toaflux.R,v 1.2 2010-08-12 09:43:58 steingod Exp $
#
# Estimates the TOA broadband irradiance.
#

toaflux <- function(tst, lat, lon, s0=1370) {

    doy <- as.numeric(strftime(as.POSIXlt(tst),"%j"))

    toa <- s0*(fm_esd(doy))*fm_deg2rad((fm_solzen(tst,lat,lon)))

    return(toa)
}
