readbioforsk <- function(file) {

    myheader <- read.table(file,nrow=1)

    stnr <- myheader[1]
    stname <- myheader[2]
    varnames <- c("Date",lapply(myheader[3:length(myheader)],as.character))

    x <- read.table(file, skip=1)
    colnames(x) <- varnames

    datetime <- strptime(x[,1],"%Y%m%d%H%M")
    #x[,1] <- datetime
    
    return(list(stname=stname,stnr=stnr,m=datetime,varnames=varnames,data=x))
}
