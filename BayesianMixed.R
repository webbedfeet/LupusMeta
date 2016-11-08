## JAGS modeling for left-truncated right-censored Weibull
## 
## JAGS parameterizes the Weibull p.d.f. as 
## f(x;nu,lambda) = nu*lambda*x^{nu-1}*exp(-lambda * x^nu)
## S(x; nu, lambda) = exp(-lambda * x^nu) is the survival function

# BUGS model -------------------------------------
## TODO: Port to STAN

# See fullmodelcts.bug

# Import data ----------------------------------------------------------

## IPD data

load('data/rda/overallipd.rda') # get the overall KM data IPD
load('data/rda/overallSumm.rda') # get the summary data IPD
load('data/rda/kmdata.rda')
load('data/rda/WardTableFinal.rda')
# if('fn' %in% search()) detach('fn')
# fn <- new.env()
# source('functions.R',local=fn)
# source('truncFunctions.R',local=fn)
# attach(fn)
reload()

## Including only studies of mixed class
classes <- dplyr::filter(basedata, study %in% as.numeric(c(names(overall.ipd), names(overall.summ))),
                         Arm=='FullStudy', Group_class=='Mixed') %>% 
  select(Author, study, Group_class, Developed, Decades, lags)

keep.overall <- overall.ipd[names(overall.ipd) %in% as.character(classes$study)]
# names(keep.overall) <- classes$study[match(names(keep.overall), classes$study)]
keep.summ <- overall.summ[names(overall.summ) %in% as.character(classes$study)]
# Remove overlap. Err conservatively on the side of summary IPDs
# ind <- intersect(names(keep.overall), names(keep.summ))
# keep.overall <- keep.overall[-match(ind, names(keep.overall))]

study.ipd <- c(keep.overall, keep.summ)

## Followup data

load('data/rda/followup.rda') # Extracted followup data
load('data/rda/WardTableFinal.rda') # Study characteristic data

library(plyr)
followup.fullstudy <- ldply(followup_data, subset, Arm=='FullStudy' & Group_class=="Mixed") # Ignore single-class studies
followup.fullstudy <- join(followup.fullstudy,
                           select(basedata, study,Arm,number, Developed, Decades, lags), 
                           by=c('study','Arm'))

# Account for lead time of disease at start of study
followup.fullstudy <- mutate(followup.fullstudy,
                             lags = ifelse(is.na(lags),0,lags),
                             maxfollowup=maxfollowup/12+lags)


studies.full.mixed <- c(names(study.ipd), as.character(followup.fullstudy$study))

save(study.ipd, keep.overall, keep.summ, followup.fullstudy, file='data/rda/mixedmcmc.rda',compress=T)

# unlink('data/fromPapers',recursive=T)
# unlink('data/xls', recursive=T)

# 
# # Format data for JAGS ----------------------------------------------------
# out <- followup.fullstudy[,c('number','maxfollowup','Events','Developed','Decades','lags')]
# data.jags <- as.data.frame(gen.jagsdata(study.ipd, classes))
# 
# names(out)[c(1,4:5)] <- c('n','geog2','yr2')
# out$isCensored2 <- as.integer(rep(1, nrow(out)))
# out$Y <- rep(NA, nrow(out))
# out$yr2 <- as.factor(out$yr2)
# out$geog2 <- as.factor(out$geog2)
# 
# data.mixed <- as.list(out)
# 
# data.mixed$N2 <- nrow(out)
# data.mixed$J <- length(levels(out$geog2))
# data.mixed$K <- length(levels(out$yr2))
# 
# names(data.jags) <- c('td','tcens','trunc','isCensored1','geog1','yr1')
# data.jags$yr1 <- as.factor(data.jags$yr1)
# data.jags$geog1 <- as.factor(data.jags$geog1)
# 
# data.mixed <- c(data.mixed, as.list(data.jags))
# data.mixed$N1 <- nrow(data.jags)
# 
# ## Make sure all the factors are numeric
# for(i in 1:length(data.mixed)){
#   if(is.factor(data.mixed[[i]])) data.mixed[[i]] <- as.numeric(data.mixed[[i]])
# }
# 
# dump('data.mixed', file='fullstudy.mixed.dat')
# 
# # JAGS model --------------------------------------------
# source('fullstudy.mixed.dat')
# library(random)
# require(rjags)
# library(doParallel)
# cl <- makeCluster(2)
# registerDoParallel(cl)
# library(foreach)
# jags.parsamples <- foreach(i=1:2, .packages=c('rjags','random')) %dopar%{
#   load.module('glm')
#   load.module('lecuyer')
#   inits <- with(data.mixed, 
#                 list(beta = matrix(0, J, K),
#                      nu = matrix(1, J, K),
#                      .RNG.name = "lecuyer::RngStream",
#                      .RNG.seed = randomNumbers(n=1, min=1, max=1e+06, col=1),
#                      td = ifelse(isCensored1==1, 100, NA), # init for censored td
#                      Y = ifelse(isCensored2==1, n, NA)# init for censored Y
# #                      ypred14 = 100,
# #                      ypred24 = 100
#                 )
#   )
#   
#   parameters <- c('lambda','nu','pr5','pr10','pr15')
#   
#   mod <- jags.model("fullmodel.bug",
#                     data = data.mixed,
#                     inits = inits,
#                     n.chains = 1, # Change to 4 after testing
#                     n.adapt = 1000)
#   #update(mod, n.iter=1000) # Burn-in
#   codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
# 
#    return(codaSamples)
# }
# stopCluster(cl)
# 
# 
# # JAGS results ------------------------------------------------------------
# 
# setwd('mcmc')
# results <- vector('list',10)
# u <- dir(pattern='rda')
# for(i in 1:10){
#   load(u[i])
#   results[[i]] <- out
# }
# rm(out)
# res.mcmc <- as.mcmc.list(lapply(results, function(x) x[[1]]))
# #plot(res.mcmc)
# 
# res.mat <- as.matrix(res.mcmc, iters=TRUE, chains=TRUE)
# res.mat <- res.mat[res.mat[,2]>6000,] 
# 
# extrStratum <- function(x){
#   # x is character vector of the form prx[i,j]
#   z <- regexpr('[12],[1234]',x, perl=TRUE)
#   res <- do.call(rbind, strsplit(regmatches(x,z),','))
#   yr.f <- function(x) switch(as.numeric(x), '1970-79','1980-89','1990-99','2000-09')
#   yr <- do.call(c,lapply(res[,2],yr.f))
#   out <- data.frame(
#     Developed =factor(ifelse(res[,1]=='1','Developed','Developing')),
#     Decades = factor(yr))
#   return(out)
# }
# 
# # Pr5
# library(reshape2)
# pr5  <- res.mat[,35:42]
# plotPosterior <- function (pr5, main='5 year survival') {
#   blah <- melt(pr5)[,-1]
#   blah[,1] <- as.character(blah[,1])
#   blah <- cbind(blah, extrStratum(blah$Var2))
#   blah <- subset(blah, Developed != 'Developing' | Decades != '1970-79')
#   library(ggplot2)
#   plt <- ggplot(blah, aes(x=Developed,y=value))+geom_boxplot()+facet_wrap(~Decades, nrow=2)+ggtitle(main)+labs(x='',y='Survival probability')+ylim(0,1)
#   print(plt)
# }
# 
# #Pr10
# 
