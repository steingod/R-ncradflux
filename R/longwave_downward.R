longwave_downward <- function(x) {

    netdown <- x$longwave+((5.67e-8)*((x$dli_temperature+273.15)^4))

    return(list(info=list(title=x$info$title,start_date=x$info$start_date,stop_date=x$info$stop_date,platform=x$info$platform,longitude=x$info$longitude,latitude=x$info$latitude,status="Modified object, net downward longwave"),time=x$time,shortwave=x$shortwave,longwave=netdown,ssi_temperature=x$ssi_temperature,dli_temperature=x$dli_temperature,battery=x$battery))
}
