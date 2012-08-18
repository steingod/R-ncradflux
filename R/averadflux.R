#
# $Id: averadflux.R,v 1.5 2012-08-18 19:12:58 steingod Exp $
#
averadflux <- function(x, period="month") {

    if (period=="month") {
        myindex <- format(x$time,"%Y%m","GMT")
        meantime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
    } else if (period=="day") {
        myindex <- format(x$time,"%Y%j","GMT")
        meantime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
    } else if (period=="diurnal") {
        myindex <- format(x$time,"%H%M","GMT")
        meantimetmp <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
        meantime <- meantimetmp
        #meantime <- as.numeric(format(meantimetmp,"%H","GMT"))+as.numeric(format(meantimetmp,"%M","GMT"))/60.
    } else if (period=="hour") {
        myindex <- format(x$time,"%Y%j%H","GMT")
        meantime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
    }
    meanssi <- tapply(x$shortwave,myindex,mean,na.rm=T)
    nvalssi <- tapply(x$shortwave,myindex,function(x) sum(!is.na(x)))
    meandli <- tapply(x$longwave,myindex,mean,na.rm=T)
    nvaldli <- tapply(x$longwave,myindex,function(x) sum(!is.na(x)))

    myinfo <- list()
    myinfo$title <- x$info$title
    myinfo$platform <- x$info$platform
    myinfo$start_date <- x$info$start_date
    myinfo$end_date <- x$info$end_date
    myinfo$longitude <- x$info$longitude
    myinfo$latitude <- x$info$latitude
    myinfo$integration_period <- period
    myinfo$integration_method <- "mean"

    return(data.frame(info=myinfo,time=meantime,mssi=meanssi,nssi=nvalssi,mdli=meandli,ndli=nvaldli))
}
