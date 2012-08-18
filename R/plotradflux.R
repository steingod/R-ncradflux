#
# $Id: plotradflux.R,v 1.7 2012-08-18 19:12:58 steingod Exp $
#
plotradflux <- function(x, ...) {

    myindex <- format(x$time,"%Y%m","GMT")
    monthlytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+tapply(x$time,myindex,mean,na.rm=T)
    ii <- order(monthlytime)
    monthlyssi <- tapply(x$shortwave,myindex,mean,na.rm=T)
    monthlydli <- tapply(x$longwave,myindex,mean,na.rm=T)
    
    def.par <- par(no.readonly=T)
    split.screen(c(2,1))
    screen(1)
    par(cex=0.7,cex.sub=0.7,mai=c(0.8,0.5,0.2,0.2))
    plot(x$time,x$shortwave,type="l",ylab="W/m²",...)
    lines(monthlytime[ii],monthlyssi[ii],type="b",col="red",lwd=3)
    if (x$info$platform == iconv("Bjørnøya",from="UTF8",to="ISO88591")) {
    #if (x$info$platform == "Bjørnøya") {
	text(ISOdatetime(2009,2,6,0,0,0,"GMT"),350,"Hungry polar bear",cex=0.8,pos=2)
	arrows(ISOdatetime(2009,2,6,0,0,0,"GMT"),350,ISOdatetime(2009,2,6,0,0,0,"GMT"),250,lwd=4,length=0.1)
    }
    title(paste("Surface Shortwave Irradiance at",x$info$platform),
    sub=paste(x$info$start_date,x$info$stop_date,sep=" - "))
    screen(2)
    par(cex=0.7,cex.sub=0.7,mai=c(0.8,0.5,0.2,0.2))
    plot(x$time,x$longwave,type="l",ylab="W/m²",...)
    lines(monthlytime[ii],monthlydli[ii],type="b",col="red",lwd=3)
    if (x$info$platform == iconv("Bjørnøya",from="UTF8",to="ISO88591")) {
    #if (x$info$platform == "Bjørnøya") {
	text(ISOdatetime(2009,2,6,0,0,0,"GMT"),350,"Hungry polar bear",cex=0.8,pos=2)
	arrows(ISOdatetime(2009,2,6,0,0,0,"GMT"),350,ISOdatetime(2009,2,6,0,0,0,"GMT"),325,lwd=4,length=0.1)
	text(ISOdatetime(2011,1,18,0,0,0,"GMT"),350,"Hungry polar bear",cex=0.8,pos=2)
	arrows(ISOdatetime(2011,1,18,0,0,0,"GMT"),350,ISOdatetime(2011,1,18,0,0,0,"GMT"),325,lwd=4,length=0.1)
    }
    title(paste("Downward Longwave Irradiance at",x$info$platform),
    sub=paste(x$info$start_date,x$info$stop_date,sep=" - "))
    par(def.par)

    return
}
