#
# NAME:
# plotyeargraph
#
# PURPOSE:
# Create a "normal" year plot.
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
# NA
#
# MODIFIED:
# Øystein Godøy, METNO/FOU, 2012-09-18 
#
# CVS_ID:
# $Id: plotyeargraph.R,v 1.2 2012-09-19 09:19:58 steingod Exp $
#  
plotyeargraph <- function(x,timezone="UTC",method=max,...) {

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

    tmpx <- as.numeric(strftime(x$time,"%j",tz=timezone))
    tmpy <- aggregate(x$shortwave,list(tmpx),method,na.rm=T)

    plot(tmpy,type="l",...)

    return(tmpy)
}
