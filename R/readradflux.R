readradflux <- function(file) {

    library(RNetCDF)
    nc <- open.nc(file)

    print.nc(nc)

    mytitle <- att.get.nc(nc,"NC_GLOBAL","title")
    myplatform <- att.get.nc(nc,"NC_GLOBAL","Platform_name")
    mystartdate <- att.get.nc(nc,"NC_GLOBAL","start_date")
    mystopdate <- att.get.nc(nc,"NC_GLOBAL","stop_date")
    mylatitude <- var.get.nc(nc,"latitude")
    mylongitude <- var.get.nc(nc,"longitude")

    mytime1 <- var.get.nc(nc,"time")
    mytime <- ISOdatetime(1970,1,1,0,0,0,"GMT")+mytime1
    myssi <- var.get.nc(nc,"ssi")
    mytempssi <- var.get.nc(nc,"ssisenstemp")
    mydli <- var.get.nc(nc,"dli")
    mytempdli <- var.get.nc(nc,"dlisenstemp")
    mybattery <- var.get.nc(nc,"battery")
    close.nc(nc)

#    split.screen(c(2,1))
#    plot(mytime,myssi,type="l")
#    screen(2)
#    plot(mytime,mydli,type="l")

    return(list(info=list(title=mytitle,platform=myplatform,start_date=mystartdate,stop_date=mystopdate,longitude=mylongitude,latitude=mylatitude),time=mytime,shortwave=myssi,longwave=mydli,ssi_temperature=mytempssi,dli_temperature=mytempdli,battery=mybattery))
}
