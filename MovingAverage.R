# Temporal moving average
library(dplyr)
reload()
# if('fn' %in% search()) detach('fn')
# fn <- new.env()
# source('functions.R',local=fn)
# source('truncFunctions.R',local=fn)
# attach(fn)

# BUGS model -------------------------------------

fullmodelcts.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
#beta[j] ~ dnorm(0.0,0.0001);
nu[j] ~ dgamma(1.0,0.0001);
lambda[j] ~ dgamma(1.0,0.0001);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
for(i in 1:N2){
p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
isCensored2[i] ~ dinterval(Y[i], Events[i]);
Y[i] ~ dbinom(p[i], n[i]);
}
}
"
writeLines(fullmodelcts.bugs, con='fullmodelcts.bug')

fullmodelcts2.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
beta[j] ~ dnorm(0.0,0.0001);
nu[j] ~ dgamma(1.0,0.0001);
lambda[j] <- exp(beta[j]);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
# for(i in 1:N2){
# p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
# isCensored2[i] ~ dinterval(Y[i], Events[i]);
# Y[i] ~ dbinom(p[i], n[i]);
# }
}
"
writeLines(fullmodelcts2.bugs, con='fullmodelcts2.bug')

# Book-keeping ------------------------------------------------------------

## See baseline.R

# Create data sets --------------------------------------------------------
load('data/rda/bookkeeping.rda')
load('data/rda/mixedmcmc.rda')
study.ipd <- c(keep.overall, keep.summ)
# jagsdata <- vector('list',length(members))
info <- dplyr::filter(basedata, study %in% as.numeric(names(study.ipd)), 
               Arm=='FullStudy', Group_class=='Mixed') %>%
  select(Author, study,Group_class,Developed,Decades,lags)
id.Mixed <- as.character(unique(dplyr::filter(basedata, Arm=='FullStudy',Group_class=='Mixed')$study))
members <- windowMembership(startYear(), endYear(), basedata$study, wins=1964:2011)
members.mixed <- lapply(members, intersect, id.Mixed)

createDatasets(members.mixed, outdir='FullMixed', info)
# members1 <- lapply(members.mixed, setdiff, c('54')) # Remove Franco
# createDatasets(members1, outdir='sansFranco',info)

# Sensitivity analysis for full data --------------------------------------
# We take the start date to be 3/4 of the time of enrollment if end of enrollment=end of followup
# We take the start date to be the end of enrollment if end of enrollment < end of followup

# members2 <- windowMembership(startYear2(), endYear(), basedata$study, wins=1964:2011)
# members2.mixed <- lapply(members2, intersect, id.Mixed)
# createDatasets(members2.mixed, outdir='FullMixedSensitivity',info)

# We now take the start date at the 1/2 way of enrollment, and give each study a max of 10 years of credit for
# the Moving Average analysis
members3 <- windowMembership(startYear(), endYear(maxduration=10), basedata$study, wins=1964:2011)
members3.mixed <- lapply(members3, intersect, id.Mixed)
createDatasets(members3.mixed, outdir='FullMixedSensitivity2',info)


design <- dplyr::filter(basedata, Arm=='FullStudy',Group_class=='Mixed') %>% select(study, Design) %>% mutate(Design = ifelse(Design=='Trial','Trial','Observational'))
id.Trial <- as.character(design$study[design$Design=='Trial'])
id.Obs  <- as.character(design$study[design$Design=='Observational'])
members.Trial <- lapply(members, intersect, id.Trial)
members.Obs <- lapply(members, intersect, id.Obs)

createDatasets(members.Trial, outdir='Trial', info, minkm=2)
createDatasets(members.Obs, outdir='Observational', info, minkm=2)

# createDatasets(members.Trial, outdir = 'Trial_1', info, minkm=4)
# createDatasets(members.Obs, outdir='Obs_1', info, minkm=5)


basedata.hq <- dplyr::filter(basedata, inception=='inception',bx.<100, !is.na(bx.), ExcludeCRF=='no', Developed=='Developed',
                      Group_class=='Mixed', Arm=='FullStudy')
# Need to add 171 and 76 to this pool (email, 3/26/14)
basedata.hq <- rbind(basedata.hq, dplyr::filter(basedata, study %in% c(171,76), Group_class=='Mixed'))
members.hq <- lapply(members, intersect, as.character(basedata.hq$study))
createDatasets(members.hq, outdir='HQ',info, minkm=2)
# createDatasets(members.hq, outdir='HQ_1', info, minkm=5)



## Class3
load('data/rda/bookkeeping.rda')
load('data/rda/class3mcmc.rda')
members3 <- windowMembership(startYear(), endYear(maxduration=10), basedata$study, wins=1964:2011)
createClassData('class3',study.ipd, class3data, class3follow, members=members)
# createClassData('class3_1', study.ipd, class3data, class3follow, members = members, minkm=5)
# createClassData('class3Sensitivity', study.ipd, class3data, class3follow, members=members3)

## Class4
load('data/rda/bookkeeping.rda')
load('data/rda/class4mcmc.rda')
members3 <- windowMembership(startYear(), endYear(maxduration=10), basedata$study, wins=1964:2011)
createClassData('class4',study.ipd, class4data, class4follow, members=members)
# createClassData('class4_1', study.ipd, class4data, class4follow, members = members, minkm=5)
# createClassData('class4Sensitivity', study.ipd, class4data, class4follow, members=members3)

## Class 5
load('data/rda/bookkeeping.rda')
load('data/rda/class5mcmc.rda')
members3 <- windowMembership(startYear(), endYear(maxduration=10), basedata$study, wins=1964:2011)
createClassData('class5',study.ipd, class5data, class5follow, members=members)
# createClassData('class5_1', study.ipd, class5data, class5follow, members = members, minkm=5)
# createClassData('class5Sensitivity', study.ipd, class5data, class5follow, members=members3)


# Sensitivity to studies at the end ---------------------------------------
# bl <- as.character(unique(dplyr::filter(basedata, Arm=='FullStudy',Group_class=='Mixed',Developed=='Developed')$study))
# blah <- lapply(members.mixed, intersect, bl)
# members.sens <- Reduce(union, blah[43:45])
# authors.sens <- unique(dplyr::filter(basedata, study %in% members.sens)%>%select(study,Author))
# authors.sens
# for(i in 1:nrow(authors.sens)){
#   members1 <- lapply(members.mixed, setdiff, c(as.character(authors.sens[i,'study'])))
#   createDatasets(members1, outdir=paste('sans',authors.sens[i,'study'],sep=''), info)
# }
# 
# members1 <- lapply(members.mixed, setdiff, c('54')) # Remove Franco
# createDatasets(members1, outdir='sansFranco',info)

# Running JAGS ------------------------------------------------------------

# #source('fullstudy.mixed.dat')
# library(random)
# require(rjags)
# load.module('glm')
# load.module('lecuyer')
# inits <- with(data.mixed, 
#               list(beta = rep(0,J),
#                    nu = rep(1, J),
#                    .RNG.name = "lecuyer::RngStream",
#                    .RNG.seed = randomNumbers(n=1, min=1, max=1e+06, col=1),
#                    td = ifelse(isCensored1==1, 100, NA), # init for censored td
#                    Y = ifelse(isCensored2==1, n, NA)# init for censored Y
#                    #                      ypred14 = 100,
#                    #                      ypred24 = 100
#               )
# )
# 
# parameters <- c('lambda','nu','pr5','pr10','pr15')
# 
# mod <- jags.model("fullmodelcts.bug",
#                   data = data.mixed,
#                   inits = inits,
#                   n.chains = 1, # Change to 4 after testing
#                   n.adapt = 1000)
# #update(mod, n.iter=1000) # Burn-in
# codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
# save(codaSamples, file='Rfile.rda',compress=T)
# 

# 
# x1 <- arrange(x1, start_date,end_date)
# plot(c(1963,2013), c(1,145), type='n',axes=F, xlab='Year',ylab='Study')
# for(i in 1:nrow(x1)) segments(y0=i, x0=x1$start_date[i], x1=x1$end_date[i])
# axis(1)
# box()
# axis(2, at=1:143, label=as.character(x1$study), las=2, cex.axis=0.5)
# abline(v=seq(1970,2010,by=5),lty=3)
# 
# addn <- subtr <- vector('list',44)
# membersKM = lapply(members, intersect,names(study.ipd))
# for(i in 1:44){
#   addn[[i]] <- setdiff(membersKM[[i+1]],membersKM[[i]])
#   subtr[[i]] <- setdiff(membersKM[[i]],membersKM[[i+1]])
# }
# 
# xx = data.frame(t(sapply(apply(books[-1,], 2, function(x) which(x==1)),range)))
# names(xx) <- c('start','end')
# xx$study <- as.character(row.names(xx))
# xx <- arrange(xx, start,end)
# 
# plot(c(1,44),c(1,nrow(x)),type='n',xlab='',ylab='', axes=F)
# for(i in 1:nrow(x)) segments(y0=i,x0=x$start[i],x1=x$end[i])
# axis(1, at=seq(4,44,by=5),labels = as.character(Windows[seq(5,45,by=5),3]))
# box()
# axis(2, at=1:143, labels=as.character(x$study), cex.axis=0.5, las=2)
# abline(v=seq(4,44,by=5),lty=3)
# 
# 
# f = function(x, tm=5){
#   s = summary(survfit(IPD(study.ipd[[x]])~1),time=tm)$surv
#   s = ifelse(length(s)==0, NA, s)
#   return(1-s)
# }
# # sapply(membersKM[[40]],f)
# # 
# empests <- function(x){
#   if(length(x)==0) return(NULL)
#   blah = dplyr::filter(basedata, study %in% as.numeric(x),Arm=='FullStudy')%.%select(study, Developed)
#   blah$pr5 <- sapply(as.character(blah$study), f)
#   blah$pr10 <- sapply(as.character(blah$study),f, tm=10)
#   blah$pr15 <- sapply(as.character(blah$study),f,tm=15)
# #   if(length(unique(blah$Developed))==1) {
# #     out <- summarise(blah, avg5=mean(pr5, na.rm=T), avg10 = mean(pr10, na.rm=T), avg15=mean(pr15, na.rm=T))
# #   } else {
#     out <- group_by(blah, Developed) %.% summarise(avg5=median(pr5, na.rm=T), avg10 = median(pr10, na.rm=T), avg15=median(pr15, na.rm=T))
# #   }
#   return(out)
# }
# 
# empricalKM = lapply(membersKM, empests)
# empricalKM <- empricalKM[-1]
# 
# grossrates <- function(x){
#   blah = dplyr::filter(basedata, study%in% as.numeric(x), Arm=='FullStudy') %.% 
#     select(study, Developed,maxfollowup, Events, number,lags) %.%
#     mutate(maxfollow=maxfollowup/12, grossrate = Events/number)
#   blah = arrange(blah, Developed, grossrate)
#   return(blah)
# }
# 
# grates <- lapply(membersNonKM, grossrates)
# 
# plot(1966+(1:44),1-outcts[[1]][[1]][,2], type='l',ylim=c(0,1),xlab='Year',ylab='Probablity of ESRD',main='Developed')
# empiricalKMed <- sapply(empricalKM, function(x) x[x$Developed=='Developed','avg5'])
# lines(1966+(1:44),empiricalKMed, col='blue' )
# legend(1990,.8, lty=1, legend=c('All studies','KM'), col=c('black','blue'))
# plot(1966+(1:44),1-outcts[[1]][[2]][,2], type='l',ylim=c(0,1),xlab='Year',ylab='Probablity of ESRD',main='Developing')
# empiricalKMing <- sapply(empricalKM, function(x) x[x$Developed=='Developing','avg5'])
# lines(1966+(1:44),empiricalKMing, col='blue' )
# legend(1990,.8, lty=1, legend=c('All studies','KM'), col=c('black','blue'))

# unlink('data/xls', recursive=T)
# unlink('data/fromPapers', recursive=T)

