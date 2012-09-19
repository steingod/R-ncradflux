#
# NAME:
# createdecimalhour
#
# PURPOSE:
# To transform date and time specifications to decimal hours.
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
# $Id: createdecimalhour.R,v 1.2 2012-09-19 09:19:58 steingod Exp $
#  

createdecimalhour <- function(x, minuteinterval=-99, ...) {

    if (class(x) != "POSIXct") return("x is of wrong class")

    hour <- as.numeric(strftime(x,"%H", ...))
    minute <- as.numeric(strftime(x,"%M", ...))

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
