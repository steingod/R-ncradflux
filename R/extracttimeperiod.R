#
# NAME:
# extracttimeperiod
#
# PURPOSE:
# To extract a time period from a long time series.
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
# Øystein Godøy, METNO/FOU, 2012-09-18 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: extracttimeperiod.R,v 1.1 2012-09-18 15:40:54 steingod Exp $
#
extracttimeperiod <- function(x,start,end) {

    if (class(start) != "POSIXct") return("start is of wrong class")
    if (class(end) != "POSIXct") return("end is of wrong class")

    newlist <- list()

    newlist$shortwave <- x$shortwave[x$time >= start & x$time <= end]
    newlist$longwave <- x$longwave[x$time >= start & x$time <= end]
    newlist$time <- x$time[x$time >= start & x$time <= end]

    newlist$info$title <- x$info$title
    newlist$info$platform <- x$info$platform
    newlist$info$longitude <- x$info$longitude
    newlist$info$latitude <- x$info$latitude
    newlist$info$status <- x$info$status
    newlist$info$start_date <- start
    newlist$info$end_date <- end

    return(newlist)
}
