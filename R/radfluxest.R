# 
# $Id: radfluxest.R,v 1.2 2010-08-12 08:56:34 steingod Exp $
#
# Various functions mainly used internally to estimate or analyse
# radiative fluxes.
#

# Convert from degrees to radians
fm_deg2rad <- function(degrees) {

    radians <- degrees*(pi/180)

    return(radians)
}

# Convert from radians to degrees
fm_rad2deg <- function(radians) {

    degrees <- radians*(180/pi)

    return(degrees)
}

# Convert from time zone to true solar time
fm_time2tst <- function(x,tz,lon) {

    #tz <- strftime(x,"%Z")
    doy <- strftime(x,"%j")

    if (tz == "GMT") {
	cmer <- 0
    } else if (tz == "CET") {
	cmer <- 15
    }
    loncorr <- floor(((lon-cmer)*240)+0.5)
    deltat <- fm_eqt(doy)

    tst <- x+loncorr+deltat

    return(tst)
}

# Estimate hour angle
fm_ha <- function(tst) {

    ha <- (tst-12.)*0.2618

    return(ha)
}

#
# Estimate solar zenith angle for given position
#
fm_solzen <- function(tst, lat, lon) {

    glat <- fm_deg2rad(lat);
    glon <- fm_deg2rad(lon);

    doy <- strftime(tst,"%j")
    hour <- strftime(tst,"%H")
    min <- strftime(tst,"%M")

    # Estimate declination
    decl <- fm_decl(doy)

    # Estimate the hour angle.
    et <- 0.170*sin(4*pi*(doy-80.)/373.)-
	0.129*sin(2*pi*(doy-8.)/355.);
    tsthour <- (hour+((min)/60.));
    hangle <- fm_ha(tsthour);
    
    # Estimate the solar zenith angle.
    coszensun <- (cos(glat)*cos(hangle)*cos(decl))+(sin(glat)*sin(decl))
    soz <- fmrad2deg((acos(coszensun)))

    return(soz);
}

#
# Convert albedo to bi-directional reflectance
#
fm_bidirrefl <- function(refl, date, solang) {

    dcorr <- fm_esd(as.numeric(format(date,"%j")))
    bidirref <- (dcorr*refl)/(cos(solang*pi/180.));

    return(bidirref)
}

#
# Distance correction earth/sun
#
fm_esd <- function(doy) {

    theta0 <- (2.*pi*doy)/365.
    d <- (1.000110+0.034221*cos(theta0)+
	    0.001280*sin(theta0)+
	    0.000719*cos(2.*theta0)+
	    0.000077*sin(2.*theta0))

    return(d)
}

#
# Estimate declination
#
fm_decl <- function(doy) {

    theta0 <- (2.*pi*doy)/365.

    decl <- 0.006918 -(0.399912*cos(theta0))+
	(0.070257*sin(theta0))-
	(0.006758*cos(2.*theta0))+
	(0.000907*sin(2.*theta0))-
	(0.002697*cos(3.*theta0))+
	(0.001480*sin(3.*theta0))


    return(decl)
}

# 
# Equation of time
#
fm_eqt <- function(doy) {

    theta0 = (2.*pi*doy)/365.;

# Estimate equation of time in seconds. The original equation gives
# it in radians which are converted to degrees by multiplication with
# 180./PI, degrees are converted to seconds by multiplication of
# 3600/15=240 (15 degrees is one hour).

    deltat <- floor(((0.000075+0.001868*cos(theta0)-
	0.032077*sin(theta0)-
	0.014615*cos(2.*theta0)-
	0.040849*sin(2.*theta0))*
	(180./pi)*240.)+0.5);
    
    return(deltat);

}

#
# Estimate the TOA radiance for a given satellite band.
#
# Currently only Ch3B.
#
# Units mW/(m² sr cm¯²)
#
fm_findsollum <- function(sunz, satname, date) {

    satCs <- fm_ch3b_identsat(satname)
    dcorr <- fm_esd(as.numeric(format(date,"%j")))
    sunzrad <- sunz*pi/180.
    sollum <- dcorr*satCs$solrad*cos(sunzrad) 

    return(sollum) 
}  

# 
# Return particulars of a specific satellite
#
# Outputs NOAA KLM A, B values, central wave length of band (needed to
# convert between brighness temperatures and radiance) and TOA radiance
# for the band in mWm-2sr-1cm-1.
#
# Check libfmutil for details and updated coefficients.
#
fm_ch3b_identsat <- function(satname) {

    if (satname == "NOAA-15"){  
	satCs.Aval= 1.621256;
	satCs.Bval= 0.998015;
	satCs.cwn= 2695.9743;
	satCs.solrad= 5.153;
    }
    else if (satname == "NOAA-16"){ 
	satCs.Aval= 1.592459;
	satCs.Bval= 0.998147;
	satCs.cwn= 2700.1148;
	satCs.solrad= 5.099;
    }
    else if (satname == "NOAA-17"){ 
	satCs.Aval= 1.702380;
	satCs.Bval= 0.997378;
	satCs.cwn= 2669.3554;
	satCs.solrad= 5.070;
    }
    else if (satname == "NOAA-18"){ 
	satCs.Aval= 1.698704;
	satCs.Bval= 0.996960;
	satCs.cwn= 2659.7952;
	satCs.solrad= 5.043;
    }
    else if (satname == "NOAA-19"){ 
	satCs.Aval= 1.67396;
	satCs.Bval= 0.997364;
	satCs.cwn= 2670.0;
	satCs.solrad= 5.070;
	}
    else {
	return("Satellite not supported")
    }

    return(list(satname=satname,Aval=Aval,Bval=Bval,cwn=cwn,solrad=solrad));
}

