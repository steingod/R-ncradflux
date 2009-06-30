averadflux <- function(x) {

    myindex <- format(x$time,"%Y%m","GMT")
    monthlytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
    ii <- order(monthlytime)
    monthlyssi <- tapply(x$shortwave,myindex,mean,na.rm=T)
    monthlydli <- tapply(x$longwave,myindex,mean,na.rm=T)

    return(data.frame(time=monthlytime,mssi=monthlyssi,mdli=monthlydli))
}
