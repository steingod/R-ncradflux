#
# $Id: readradflux.R,v 1.4 2011-12-06 18:43:20 steingod Exp $
#
readradflux <- function(file,readUVMED=FALSE,readSSITEMP=TRUE,readDLITEMP=TRUE) {

    library(RNetCDF)
    nc <- open.nc(file)

    print.nc(nc)

    myuvmed <- NULL
    mytempssi <- NULL
    mytempdli <- NULL

    mytitle <- att.get.nc(nc,"NC_GLOBAL","title")
    myplatform <- att.get.nc(nc,"NC_GLOBAL","Platform_name")
    mystartdate <- att.get.nc(nc,"NC_GLOBAL","start_date")
    mystopdate <- att.get.nc(nc,"NC_GLOBAL","stop_date")
    mylatitude <- var.get.nc(nc,"latitude")
    mylongitude <- var.get.nc(nc,"longitude")

    Encoding(myplatform) <- "latin1"

    mytime1 <- var.get.nc(nc,"time")
    mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+mytime1
    myssi <- var.get.nc(nc,"ssi")
    if (readSSITEMP) {
	mytempssi <- var.get.nc(nc,"ssisenstemp")
    }
    mydli <- var.get.nc(nc,"dli")
    if (readDLITEMP) {
	mytempdli <- var.get.nc(nc,"dlisenstemp")
    }
    if (readUVMED) {
	myuvmed <- var.get.nc(nc,"uvmed")
    }
    mybattery <- var.get.nc(nc,"battery")
    close.nc(nc)

#    split.screen(c(2,1))
#    plot(mytime,myssi,type="l")
#    screen(2)
#    plot(mytime,mydli,type="l")

    return(list(info=list(title=mytitle,platform=myplatform,start_date=mystartdate,stop_date=mystopdate,longitude=mylongitude,latitude=mylatitude),time=mytime,shortwave=myssi,longwave=mydli,ssi_temperature=mytempssi,dli_temperature=mytempdli,uvmed=myuvmed,battery=mybattery))
}
