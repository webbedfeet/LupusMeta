##IPD to survival estimates
reload()
library(gdata)
library(stringr)
if('fn' %in% search()) detach('fn')
fn <- new.env()
source('functions.R',local=fn)
source('truncFunctions.R',local=fn)
source('KMcleaning.R', local=fn)
attach(fn)

kmdata <- read.xls('data/Summary2.xlsx', sheet='Summary')
# kmdata$arm <- str_trim(kmdata$arm)

load('data/rda/WardTableFinal.rda')

# Compute dates -----------------------------------------------------------

kmdata2 <- merge(kmdata,
                basedata[basedata$Arm=='FullStudy',c('study','Author','Country','pubdate',
                            'inception','Time0','yr_of_study','lags',
                            'esrd','esrdmean','esrdfu..m.', 'sledur..yr.','LNdur', 'Developed','Decades')],
                by.x=c('studyno'),by.y=c('study'), all.x=T)



save(kmdata2,file='data/rda/kmdata.rda')

# Create IPD for each study -----------------------------------------------

Summ <- read.xls('data/Summary2.xlsx',sheet='Summary')
Summ <- Summ[order(Summ$Study),]
load('data/rda/kmdata2.rda') # see DataMunging.R
## kmdata is a list of KM csv files

#Calibrate Ward and Moon
# x <- read.csv('data/fromPapers/Ward.csv')
# x$Year = x$Year - min(x$Year)
# write.csv(x, 'data/fromPapers/Ward.csv', row.names=F)
# x <- read.csv('data/fromPapers/Moon.csv')
# x$Year <- x$Year - min(x$Year)
# write.csv(x[,-1], 'data/fromPapers/Moon.csv',row.names=F)


ipd.data <- vector('list',length(kmdata))
for(i in 1:length(kmdata)){
  print(i)
  stdy <- as.numeric(names(kmdata)[i])
  cleaned <- KMclean(kmdata[[i]], f.n.risk(stdy))
  ipd.data[[i]] <- get.IPD(cleaned,f.n.risk(stdy))
}
names(ipd.data) <- names(kmdata)
save(ipd.data, file='data/rda/cleanKM.rda', compress=T)

# for(i in 1:nrow(Summ)){
#   print(i)
#   try(ipd.data[[i]] <- get.IPD(get.KM(i), get.n.risk(i)))
# }

## ID where the overall data lies
labs <- lapply(ipd.data, names)
ind.overall=sapply(labs, function(x) ifelse(any(str_detect(x,'d.times')| str_detect(x,'[oO]verall')),'yes','no'))


overall.ipd <- vector('list',length(ipd.data))
for(i in 1:length(ipd.data)){
  labs=names(ipd.data[[i]])
  if(any(str_detect(labs,'d.times'))){
    overall.ipd[[i]] <- ipd.data[[i]]
  } else if(any(str_detect(labs,'[oO]verall'))){
    ind <- grep('overall',labs, ignore.case=T)
    overall.ipd[[i]] <- ipd.data[[i]][[ind]]
  } else {
    overall.ipd[[i]] <- pool.ipd(ipd.data[[i]])
  }
}
names(overall.ipd) <- names(ipd.data)

save(overall.ipd, file='data/rda/overallipd.rda')

# unlink('data/fromPapers', recursive=T)
# unlink('data/xls',recursive=T)

# # Estimate Weibull parameters ---------------------------------------------
# 
# load('data/rda/kmdata.rda')
# Summ <- kmdata2[order(kmdata2$Study),]
# get.lag <- function(i) Summ$lag[i]
# 
# survtimes <- matrix(NA,nrow=nrow(Summ), ncol=3)
# survtimes.enhanced <- vector('list',nrow(Summ))
# library(eha)
# P <- list()
# for(i in 1:nrow(Summ)){
#   #if(i %in% c(11,12,27))next
#   print(i)
#   IPD <- overall.ipd[[i]]
#   
#   if(length(IPD$d.times)==0)next
#   s <- create.Surv(IPD, get.lag(i))
#   try(mod <- phreg(s~1, dist='weibull'))
#   if(is.numeric(mod))next
#   params <- list(coef=mod$coef,vars=mod$var);
#   P[[i]] <- mod$coef
#   survtimes[i,] <- pweibull(c(5,10,15),scale=exp(params$coef[1]),shape=exp(params$coef[2]), lower.tail=F);
#   survtimes.enhanced[[i]] <- survtime.ci(params)
#   
# }
# 
# out <- data.frame(cbind(survtimes, Summ[,c('Study','yearofstudy','Developed')]))
# names(out) <- c('yr5','yr10','yr15','Study','StudyYear','Developed')
# out$Developed <- factor(out$Developed)
# out$yr1 <- cut(out$StudyYear, seq(1970,2010,by=10),right=F)
# levels(out$yr1) <- c('1970-1979','1980-1989','1990-1999','2000-2009')
# 
# ## Lets get sample sizes and weight graph
# Summ <- kmdata2[order(kmdata2$Study),]
# overall.n <- rep(0,40)
# for(i in 1:40){
#   n.risk <- get.n.risk(i)
#   if(length(n.risk)==1){
#     overall.n[i] <- n.risk
#   } else if(any(str_detect(names(n.risk),'[oO]verall'),na.rm=T)){
#     ind <- grep('overall',names(n.risk),ignore.case=T)
#     overall.n[i] <- n.risk[ind]
#   } else {
#     overall.n[i] <- sum(n.risk)
#   }
# }
# 
# out$n <- overall.n
# out$n.event <- sapply(overall.ipd, function(l) length(l$d.times))
# 
# max.followup <- function(ipd){
#   max(do.call(c,ipd))
# }
# 
# 
# max.follow <-sapply(overall.ipd,max.followup)+sapply(1:nrow(Summ),get.lag)
# out.noextra <- out
#   out.noextra$yr5 <- ifelse(max.follow<5,NA,out.noextra$yr5)
#   out.noextra$yr10 <- ifelse(max.follow<10,NA,out.noextra$yr10)
#   out.noextra$yr15 <- ifelse(max.follow<15,NA,out.noextra$yr15)
# 
# ## Some studies are subclass-only
# blah <- merge(basedata, kmdata2, by.x=c('study','Arm'),by.y=c('studyno','arm'))
# cols <- grep('class',names(blah))
# classonly=apply(blah[,cols],1, function(x) any(x==100, na.rm=T))
# overall.studies <- blah$Study[!classonly]
# 
# 
# out.overall <- subset(out, Study %in% overall.studies)
# out.noextra.overall <- subset(out.noextra, Study %in% overall.studies)
# 
# 
# out2 <- melt(out.overall[,c('Study','StudyYear','yr5','yr10','yr15','Developed','n','n.event')], 
#              id=c('Study','StudyYear','Developed','n','n.event'))
# out2.noextra <- melt(out.noextra.overall[,c('Study','StudyYear','yr5','yr10','yr15','Developed','n','n.event')], 
#                      id=c('Study','StudyYear','Developed','n','n.event'))
# 
# 
# levels(out2.noextra$variable) <- c('5 years','10 years','15 years')
# ggplot(out2.noextra, aes(x=StudyYear,y=value))+
#   geom_point(aes(size=n.event))+ylim(0,1)+
#   facet_wrap(~variable+Developed, ncol=2)+labs(size='No of events', y='Survival probability')
# 
# ## Extract confidence intervals
# goodres <- !(sapply(survtimes.enhanced,is.null))
# bl <- data.frame(Study=out$Study[goodres], studyno=(1:40)[goodres],do.call(rbind, lapply(survtimes.enhanced, function(l) l$yr10)))
# names(bl)[-1] <- c('StudyNo','yr10','ymin','ymax')
# bl <- subset(bl, Study %in% overall.studies)
# ggplot(bl, aes(x=Study,y=yr10,ymin=ymin, ymax=ymax))+geom_pointrange()+
#   labs(y='10 year survival')+ coord_flip()
# 
# 
# # Problem children --------------------------------------------------------
# 
# indx <- c(12,15,20,24,28,30,36,41,42,45,46,48)
# Summ$File[indx]












