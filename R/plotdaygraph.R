#
# Content:
# Plot functions for examination of inconsistencies in solar fluxes.
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
# $Id: plotdaygraph.R,v 1.1 2012-08-18 19:12:58 steingod Exp $
#
plotdaygraph <- function(x,timezone="UTC",interval=10,decimal=2,method=max,...) {

    if (timezone == "UTC") {
        tst <- utc2tst(x$time,x$info$longitude)
    } else if (timezone == "CET") {
        tst <- cet2tst(x$time,x$info$longitude)
    } else if (timezone =="TST") {
        tst <- x$time
    } else {
        return("Timezone not supported")
    }
    tmpx <- round(createdecimalhour(tst, interval),decimal)
    tmpy <- aggregate(x$shortwave,list(tmpx),method,na.rm=T)

    plot(tmpy,type="l",...)
    abline(v=12)

    return(tmpy)
}

createdecimalhour <- function(x, minuteinterval=-99) {

    hour <- as.numeric(strftime(x,"%H",tz="GMT"))
    minute <- as.numeric(strftime(x,"%M",tz="GMT"))

    # Round minutes to intervals
    # Fungerer ikke optimalt rundt midnatt o.l.... Øystein Godøy,
    # METNO/FOU, 2012-08-18 
    if (minuteinterval > 0) {
        roundedminute <- round((minute/minuteinterval))*minuteinterval
        minute <- roundedminute
    }

    tmp <- hour+(minute/60.)

    return(tmp)
}
