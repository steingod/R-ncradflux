#
# NAME:
# dumpresults
#
# PURPOSE:
# Dumps results on monthly files using write.table. This function only
# works on the output of averadflux.
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
# Remember to create a help page for this function. It is yet
# undocumented!!
#
# BUGS:
# NA
#
# AUTHOR:
# Øystein Godøy, METNO/FOU, 2011-04-04 
#
# MODIFIED:
# NA
#
# CVS_ID:
# $Id: dumpresults.R,v 1.2 2012-04-13 22:35:48 steingod Exp $
#

dumpresults <- function(path="./",x) {

    mytime <- x$time
    myfactor <- factor(strftime(mytime,"%Y%m",tz="GMT"))
    mylevels <- levels(myfactor)

    ##Encoding(x$info.platform) <- "latin1"
    ##mylocation <- as.character(x$info.platform[1])
    ##iconv(mylocation, "ISO_8859-1", "UTF-8")

    if (x$info.platform[1]=="Bj\xf8rn\xf8ya") {
        location <- "bjornoya"
    } else if (x$info.platform[1]=="Jan Mayen") {
        location <- "janmayen"
    } else if (x$info.platform[1]=="Hopen") {
        location <- "hopen"
    } else if (x$info.platform[1]=="Ekofisk oil rig") {
        location <- "ekofisk"
    }

    for (mymonth in mylevels) {
        mydata <- x[myfactor==mymonth,c("time","mssi","nssi","mdli","ndli")]
        outfile <- paste(path,"/","radflux_",location,"_",mymonth,".txt",sep="")
        write.table(mydata,outfile,na="-999.",row.names=F)
    }
}
