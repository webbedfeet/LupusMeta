
# Validation of digitized KM data -----------------------------------------

library(plyr)
library(gdata)
inputdir <- 'data/fromPapers'
csvfiles  <- dir(inputdir, pattern='csv')
csvfiles <- csvfiles[-grep('~$',csvfiles)]

bl <- read.xls('data/Summary.xlsx',sheet=1)

f <- csvfiles[15]


inputdir <- 'data/fromPapers'
csvfiles  <- dir(inputdir, pattern='csv')
csvfiles <- csvfiles[-grep('~$',csvfiles)]
require(gdata)
bl <- read.xls('data/Summary.xlsx',sheet=1)

f <- csvfiles[30]

dat0 <- read.csv(file.path(inputdir,f), header=F)
dat <- dat0
ind <- grep(f, bl$File)
n.risk0 <- as.numeric(unlist(strsplit(bl$Total.number.at.risk[ind],',')))
n.risk <- NULL
if(bl$Details[ind]=='Y') n.risk <- read.xls('data/Summary.xlsx',sheet=bl$Sheet[ind])


if(bl$One.arm.[ind]=='N') dat <- dlply(dat,~V1, KMvalid, strip_splits)

KMvalid <- function (dat) {
  if(ncol(dat)==3) dat=dat[,-1]
  dat <- dat[order(dat[,1]),]
  dat[,2] <- pmin(1,dat[,2])
  dat[,2] <- pmax(0, dat[,2])
  for(i in 2:nrow(dat)){
    if(dat[i,2]>dat[i-1,2]){
      dat[i,2] <- dat[i-1,2]
    }
    if(dat[i-1,2]-dat[i,2] < 1/n.risk0) dat[i,2] <- dat[i-1,2] # min jump is 1/n
  }
  return(dat)
}

dat <- dat[order(dat[,1]),]
dat[,2] <- pmin(1,dat[,2])
dat[,2] <- pmax(0, dat[,2])
for(i in 2:nrow(dat)){
  if(dat[i,2]>dat[i-1,2]){
    dat[i,2] <- dat[i-1,2]
  }
  if(dat[i-1,2]-dat[i,2] < 1/n.risk0) dat[i,2] <- dat[i-1,2] # min jump is 1/n
}



