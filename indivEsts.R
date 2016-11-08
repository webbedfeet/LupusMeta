## Creating graphs showing individual estimates by strata
reload()
load('data/rda/WardTableFinal.rda')
dat <- basedata %>%
  mutate(starts=beginYear(), ends=endYear(), yr_of_study=startYear(), yr_of_study_end=endYear()) %>% 
  dplyr::filter(Arm=='FullStudy', Group_class=='Mixed') %>% 
  select(study, starts, ends, yr_of_study, yr_of_study_end, Developed)

load('data/rda/mixedmcmc.rda')
study.ipd <- c(keep.overall, keep.summ)

create.Surv2 <- function(compute.lst, lag=0){
  if(!('d.times' %in% names(compute.lst))){
    lapply(compute.lst, create.Surv)
  }
  require(survival)
  len = sapply(compute.lst, length)
  indic <- rep(c(1,0), len)
  compute.lst$d.times[compute.lst$d.times==0] <- 0.0001
  times <- do.call(c, compute.lst)+lag
  return(Surv(times, indic))
}

survs <- list()
for(i in 1:length(study.ipd)){
  l <- get.lag(names(study.ipd)[i], basedata)$lag
  survs[[i]] <- create.Surv2(study.ipd[[i]], l)
}
names(survs) <- names(study.ipd)

getProb <- function(sv, yr=5){
  require(eha)
  maxtime = max(sv[,'time'])# Assumes just end times
  if(maxtime < min(yr)) return(NULL)
  p <- exp(coef(weibreg(sv~1)))
  probs <- as.list(pweibull(yr[yr <= maxtime], scale = p[1], shape = p[2]))
  names(probs) <- paste0('yr', yr[yr <= maxtime])
  probs <- as.data.frame(probs)
  return(probs)
}

library(tidyr)
pt_ests <- ldply(survs, getProb, yr=c(5,10,15)) %>% 
  mutate(study = as.numeric(.id)) %>% 
  left_join(dat) %>% 
  select(-.id) %>% 
  gather(yr,prob, yr5:yr15) %>% mutate(Developed=as.factor(Developed))
levels(pt_ests$yr) <- c('5 year','10 year', '15 year')

# Add followup data -------------------------------------------------------

pt_ests_follow <- followup.fullstudy %>% 
  mutate(prob = Events/number, yr5 = ifelse(maxfollowup>=5, prob,NA), 
         yr10 = ifelse(maxfollowup >= 10, prob, NA),
         yr15 = ifelse(maxfollowup >= 15, prob, NA)) %>% 
  dplyr::select(study, yr5, yr10, yr15) %>% 
  left_join(dat) %>% 
  dplyr::filter(!is.na(yr5)) %>% 
  gather(yr, prob, yr5:yr15) %>% 
  unique()

levels(pt_ests_follow$yr) <- c('5 year','10 year','15 year')

ests <- list('Time-to-event' = pt_ests, 'Followup' = pt_ests_follow)

ests <- ldply(ests)
ests <- ests %>% mutate(type=as.factor(.id))
library(ggplot2)
plt <- ggplot(ests, aes(x=prob, ymin=yr_of_study, ymax=yr_of_study_end,group=type, linetype=type))+
  geom_linerange()+
  facet_grid(Developed~yr)+
  scale_linetype_manual(values=c(2,1),breaks=c('Time-to-event','Followup'))+
  labs(x='Probability of ESRD', y='Year',linetype='Study type') +
  coord_flip() +
  theme_bw()+
  theme(legend.position='none')

pdf('graphsForPaper2/SupplFigure3.pdf', width=10, height = 7)
print(plt)
dev.off()





