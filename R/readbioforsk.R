#
# $Id: readbioforsk.R,v 1.3 2010-08-11 14:37:59 steingod Exp $
#
# Bioforsk data are valid for the hour after the time registered.
#
readbioforsk <- function(file) {

    myscipen <- getOption("scipen")
    options(scipen=5)

    myheader <- read.table(file,nrow=1)

    stnr <- myheader[1]
    stname <- myheader[2]
    varnames <- c("Date",lapply(myheader[3:length(myheader)],as.character))

    x <- read.table(file, skip=1)
    colnames(x) <- varnames

    datetime <- lapply(as.character(x[,1]), strptime, "%Y%m%d%H%M%S", tz="GMT")
    x[,1] <- sapply(datetime,as.POSIXct)

    options(scipen=myscipen)
    
    return(list(stname=stname,stnr=stnr,tz="GMT",data=x))
}
