# Fake data for summary survival information ------------------------------
# Restart R session to get rid of MASS
# 
# Load data
load('data/rda/WardTableFinal.rda')
load('data/rda/summarySurv.rda')
if(length(grep('fn',search()))>0) detach('fn')
fn <- new.env()
source('functions.R',local=fn)
source('truncFunctions.R',local=fn) # This masks the functions from the 
attach(fn)                          # workspace
library(dplyr)
library(plyr)
## All the functions needed for this are in functions.R. Truncated distribution
## functions are in truncFunctions.R

for(i in 1:length(summaryData)) summaryData[[i]] <- transform(summaryData[[i]], study = rep(names(summaryData)[i],nrow(summaryData[[i]])))

fnlist = list(n.risk=get.n.risk, n.events=get.tot.events, maxfollowup=get.maxfollowup, lags=get.lag)

getIPD.summ <- function(summData, arm='FullStudy'){
  summData <- subset(summData, Arm==arm)
  stdy <- unique(summData$study)
  lags <- as.numeric(get.lag( stdy, basedata)$lag)
  arg <- list(summData=summData, params=weib.param(summData, Lag=lags))
  arg <- c(arg, llply(fnlist, do.call, list(basedata=basedata, stdy=stdy, arm=arm)))
  arg$n.risk = arg$n.risk$number
  arg$lags <- arg$lags$lags
  return(
    do.call(
      summ2IPD, arg))
}

overall.summ = llply(summaryData, getIPD.summ)
save('overall.summ',file='data/rda/overallSumm.rda',compress=T)

unlink('data/fromPapers',recursive=T)
unlink('data/xls', recursive=T)
