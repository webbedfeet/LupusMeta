dat <- read.xls('data/Franco data.xls',stringsAsFactors=F)
dat[dat=='na'|dat==''] <- NA
franco <- data.frame(Time=as.numeric(dat$time.to.event)/365*12,
                     Event=dat[,1])
write.csv(franco,file='data/IPD/Franco.ipd',row.names=F)