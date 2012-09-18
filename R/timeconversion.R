#
# Content:
# Time manipulation functions, most are taken from libfmutil.
#
# PURPOSE:
# NA
#
# REQUIREMENTS:
# NA
#
# INPUT:
# NA
#
# OUTPUT:
# NA
#
# NOTES:
# NA
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2012-08-18 
#
# MODIFIED:
# NA
#
# ID:
# $Id: timeconversion.R,v 1.2 2012-09-18 15:03:56 steingod Exp $
#
loncorr <- function(centralmeridian, longitude) {

    corr <- floor(((longitude-centralmeridian)*240.)+0.5)

    return(corr)
}

equationoftime <- function(doy) {

    theta0 <- (2.*pi*doy)/365.

    # Estimate equation of time in seconds. The original equation gives
    # it in radians which are converted to degrees by multiplication with
    # 180./PI, degrees are converted to seconds by multiplication of
    # 3600/15=240 (15 degrees is one hour).
    deltat <- floor(((0.000075+0.001868*cos(theta0)
	-0.032077*sin(theta0)
	-0.014615*cos(2.*theta0)
	-0.040849*sin(2.*theta0))
	*(180./pi)*240.)+0.5)
    
    return(deltat)
}

utc2tst <- function(utc, lon) {

    utcsec <- as.POSIXlt(utc)

    mytime <- utcsec+loncorr(0,lon)
    deltat <- equationoftime(as.POSIXlt(mytime)$yday)

    tst <- as.POSIXct(mytime+deltat)

    return(tst)
}

cet2tst <- function(cet, lon) {

    cetsec <- as.POSIXlt(cet)

    mytime <- cetsec+loncorr(15,lon)
    deltat <- equationoftime(as.POSIXlt(mytime)$yday)

    tst <- as.POSIXct(mytime+deltat)

    return(tst)
}
