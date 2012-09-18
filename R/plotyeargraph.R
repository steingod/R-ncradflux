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
# $Id: plotyeargraph.R,v 1.1 2012-09-18 15:03:56 steingod Exp $
#  
plotyeargraph <- function(x,timezone="GMT",method=max,...) {

    tmpx <- as.numeric(strftime(x$time,"%j",tz=timezone))
    tmpy <- aggregate(x$shortwave,list(tmpx),method,na.rm=T)

    plot(tmpy,type="l",...)

    return(tmpy)
}
