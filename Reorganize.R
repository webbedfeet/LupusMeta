## Re-organizing the KM data and study information
## Each study should have only one KM file and at.risk sheet, stratified if necessary
## We only have 40 studies.

library(gdata)
library(plyr)
library(stringr)
library(xlsx)

kmdata <- read.xls('data/Summary.xls',sheet='Summary')
kmdata$arm <- str_trim(kmdata$arm)

multrows <- as.numeric(names(which(table(kmdata$studyno)>1)))
singletons <- setdiff(kmdata$studyno, multrows)

kmdata2 <- kmdata[kmdata$studyno %in% singletons,]

write.xlsx(kmdata2, file='data/Summary2.xlsx', row.names=F, sheetName='Summary')

unqstudies <- unique(multrows)

rm.first.word <- function(ch){
  out <- if(length(ch)==1){
    ''
  } else {
    str_c(str_trim(ch[-1]),collapse=' ')
  }
}

get.info <- function(i){kmdata$Study[kmdata$studyno==unqstudies[i]]}
merge.data <- function(i){
  indx <- which(kmdata$studyno==unqstudies[i])
  dat <- lapply(file.path('data','fromPapers',kmdata$File[indx]), read.csv)
  labs <- sapply(str_split(kmdata$Study[indx],' '), rm.first.word)
  if(all(str_detect(labs,'Italiano'))) labs=stringr::word(labs,start=2,end=-1) # Take off Italiano
  names(dat) <- labs
  if(any(sapply(dat,ncol)==3)){
    dat$overall <- cbind('Group'=rep('overall',nrow(dat$overall)),dat$overall)
    outdat <- do.call(rbind,dat)
  } else {
    names(dat)[names(dat)==""] <- 'overall'
    outdat <- ldply(dat); names(outdat)[1] <- 'Group'
  }
  return(outdat)
}
merge.tot.at.risk <- function(i){
  indx <- which(kmdata$studyno==unqstudies[i])
  tot.at.risk <- str_c(kmdata$TotalAtRisk[indx],collapse=',')
  return(tot.at.risk)
}

merge.tot.events <- function(i){
  indx <- which(kmdata$studyno==unqstudies[i])
  tot.events <- ifelse(any(kmdata$TotalNumberOfEvents[indx]!=''),
                       str_c(kmdata$TotalNumberOfEvents[indx],collapse=','),
                       '')
  return(tot.events)
}

merge.at.risk <- function(i){
  indx <- which(kmdata$studyno==unqstudies[i])
  dat <- lapply(kmdata$Sheet[indx], function(x) read.xls('data/Summary.xls',sheet=x))
  labs <- sapply(str_split(kmdata$Study[indx],' '), rm.first.word)
  if(all(str_detect(labs,'Italiano'))) labs=stringr::word(labs,start=2,end=-1) # Take off Italiano
  names(dat) <- labs
  if(any(sapply(dat,ncol)==3)){
    dat$overall <- cbind('Group'=rep('overall',nrow(dat$overall)),dat$overall)
    outdat <- do.call(rbind,dat)
  } else {
    names(dat)[names(dat)==""] <- 'overall'
    outdat <- ldply(dat); names(outdat)[1] <- 'Group'
  }
  return(outdat)
}



print(labs)
names(dat) <- labs
outdat <- ldply(dat); names(outdat)[1] <- 'Group'

i <- 1
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Gruppo\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Gruppo Italiano'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i)
info$File <- 'Gruppo composite.csv'
info$Sheet <- 'Italiano'
info$arm <- ''
at.risk <- merge.at.risk(i)
write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Italiano', append=T, row.names=F)
kmdata2 <- rbind(kmdata2, info)

i <- 2
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Abraham\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Abraham'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i)
info$File <- 'Abraham composite.csv'
info$Sheet <- 'Abraham'
info$arm <- ''
# No at.risk
#write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Abraham', append=T)
kmdata2 <- rbind(kmdata2, info)

i <- 3
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Alarcon\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Alarcon'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i)
info$File <- 'Alarcon composite.csv'
info$Sheet <- 'Alarcon'
info$arm <- ''
# No at.risk
#write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Alarcon', append=T)
kmdata2 <- rbind(kmdata2, info)

i <- 4
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Beji\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Beji'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i)
info$File <- 'Beji composite.csv'
info$Sheet <- 'Beji'
info$arm <- ''
# No at.risk
#write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Beji', append=T)
kmdata2 <- rbind(kmdata2, info)

i <- 5
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Donadio\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Donadio'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i); info$TotalNumberOfEvents <- '111,,,,'
info$File <- 'Donadio composite.csv'
info$Sheet <- 'Donadio'
info$arm <- ''
at.risk=merge.at.risk(i)
write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Donadio', append=T, row.names=F)
kmdata2 <- rbind(kmdata2, info)

i <- 6
get.info(i)
dat <- merge.data(i)
write.csv(dat, file=file.path('data','fromPapers','Mok\ composite.csv'), row.names=F)
info <- kmdata[kmdata$studyno==unqstudies[i],][1,]
info$Study <- 'Mok'
info$OneArm <- 'N'
info$TotalAtRisk <- merge.tot.at.risk(i)
info$TotalNumberOfEvents <- merge.tot.events(i); 
info$File <- 'Mok composite.csv'
info$Sheet <- 'Mok'
info$arm <- ''
at.risk=merge.at.risk(i)
write.xlsx(at.risk,file='data/Summary2.xlsx', sheetName='Mok', append=T, row.names=F)
kmdata2 <- rbind(kmdata2, info)

for(i in 1:34){
  if(kmdata2$Details[i]=='Y'){
    at.risk=read.xls('data/Summary.xls',sheet=kmdata2$Sheet[i])
    write.xlsx(at.risk, file='data/Summary2.xlsx',sheetName=kmdata2$Sheet[i], append=T, row.names=F)
  }
}

write.xlsx(kmdata2, file='data/Summary2.xlsx',sheetName='Summary2', row.names=F,append=T)
wb <- loadWorkbook(file='data/Summary2.xlsx')
removeSheet(wb, sheetName='Summary')
saveWorkbook(wb, file='data/Summary2.xlsx')
