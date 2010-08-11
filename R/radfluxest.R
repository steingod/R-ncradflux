# 
# IKKE FERDIG!!!!
#
# Dette er ikke ferdig, må kunne estimere TOA innstråling for
# kvalitetskontroll av kortbølget innstråling. Dvs, trenger også
# estimering av solens senitvinkel for posisjoner o.l.
#
# $Id: radfluxest.R,v 1.1 2010-08-11 14:37:59 steingod Exp $
#
fm_bidirrefl <- function(refl, date, solang) {

    dcorr <- fm_esd(as.numeric(format(date,"%j")))
    bidirref <- (dcorr*refl)/(cos(solang*pi/180.));

    return(bidirref)
}

fm_esd <- function(doy) {

    theta0 <- (2.*pi*doy)/365.
    d <- (1.000110
	+0.034221*cos(theta0)
	+0.001280*sin(theta0)
	+0.000719*cos(2.*theta0)
	+0.000077*sin(2.*theta0))

    return(d)
}

fm_findsollum <- function(solang, satname, date) {

    satCs <- fm_ch3b_identsat(satname)
    dcorr <- fm_esd(as.numeric(format(date,"%j")))
    solangrad <- solang*pi/180.
    sollum <- dcorr*satCs$solrad*cos(solangrad) 

    return(sollum) 
}  

