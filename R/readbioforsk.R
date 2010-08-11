readbioforsk <- function(file) {

    myheader <- read.table(file,nrow=1)

    stnr <- myheader[1]
    stname <- myheader[2]
    varnames <- c("Date",lapply(myheader[3:length(myheader)],as.character))

    x <- read.table(file, skip=1)
    colnames(x) <- varnames

    datetime <- lapply(as.character(x[,1]), strptime, "%Y%m%d%H%M%S", tz="GMT")
    #x[,1] <- as.vector(datetime)
    
    return(list(stname=stname,stnr=stnr,m=datetime,varnames=varnames,data=x))
}
