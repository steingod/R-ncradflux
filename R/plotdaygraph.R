#
# Content:
# Plot a "normal" day.
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
# $Id: plotdaygraph.R,v 1.4 2012-09-18 15:40:54 steingod Exp $
#
plotdaygraph <- function(x,timezone="UTC",interval=10,decimal=2,method=max,...) {

    if (timezone == "UTC") {
        tst <- utc2tst(x$time,x$info$longitude)
        timezone <- "GMT"
    } else if (timezone == "CET") {
        tst <- cet2tst(x$time,x$info$longitude)
    } else if (timezone =="TST") {
        tst <- x$time
    } else {
        return("Timezone not supported")
    }
    tmpx <- round(createdecimalhour(tst, interval, tz=timezone),decimal)
    tmpy <- aggregate(x$shortwave,list(tmpx),method,na.rm=T)

    plot(tmpy,type="l",...)
    abline(v=12)

    return(tmpy)
}

