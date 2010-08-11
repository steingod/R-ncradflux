#
# $Id: evalradflux.R,v 1.1 2010-08-11 14:37:59 steingod Exp $
#
evalradflux <- function(x,parameter="SSI",integration="DAILY",subset) {

    if (parameter=="SSI") {
	tmp <- data.frame(time=x$time,par=x$shortwave)
    } else if (parameter=="DLI") {
	tmp <- data.frame(time=x$time,par=x$longwave)
    } else {
	return("Parameter is not supported!")
    }

    if (integration=="MONTHLY") {
	myindex <- format(x$time,"%m","GMT")
	mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(tmp$time,myindex,mean,na.rm=T)
    } else if (integration=="DAILY") {
	myindex <- format(x$time,"%H","GMT")
	mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(tmp$time,myindex,mean,na.rm=T)
    } else if (integration=="DAILYCONT") {
	myindex <- format(x$time,"%H:%M","GMT")
	mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(tmp$time,myindex,mean,na.rm=T)
    } else {
	return("Integration period is not supported!")
    }

    ii <- sort(order(mytime))
    mydata <- tapply(tmp$par,myindex,mean,na.rm=T)

    plot(ii,mydata[ii],type="b",ylab="W/m²",xlab="UTC",lwd=2)
    if (integration=="DAILY") {
	abline(v=12)
    } else if (integration=="DAILYCONT") {
	abline(v=720)
    }
    title(parameter)

    return(data.frame(time=mytime,data=mydata,ii=ii))
}
