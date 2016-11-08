library(testthat)
require(dplyr)
library(readr)
library(readxl)
options(stringsAsFactors=F)


load('data/rda/WardTableFinal.rda') # Provides basedata
untar('data/xls.tar.gz',exdir='data')
untar('data/fromPapers.tar.gz', exdir='data')
Summ <- read_excel('data/xls/Summary.xlsx',sheet='Summary')
Summ <- Summ[order(Summ$Study),]
Summ[Summ==""] <- NA

datadir <- 'data/fromPapers/'


get.study.info <- function(indx, basedata=Summ){
  if(is.character(indx)) indx <- grep(indx, basedata$Study)
  out <-basedata[indx,c('Study','KMavailable','TotalAtRisk','TotalNumberOfEvents','Details')]
  out <- data.frame(out)
  names(out) <- c('Study','KM?','n.risk','n.events','Details')
  return(out)
}

get.n.risk <- function( stdy, basedata,  arm=TRUE, developed=FALSE){
  if(is.character(stdy)) stdy <- as.numeric(stdy)
  if(is.logical(arm)){
    if(developed){
      dat <- dplyr::filter(basedata, study %in% stdy)%>% select(study,Arm, number,Developed)
    } else {
      dat <- dplyr::filter(basedata, study %in% stdy)%>% select(study,Arm, number)      
    }
   return(dat$number)
 } else {
   if(developed){
     dat <- dplyr::filter(basedata, study %in% stdy, Arm==arm) %>% select(study,number, Developed)
   }
   else{
     dat <- dplyr::filter(basedata, study %in% stdy, Arm==arm) %>% select(study,number)
   }
  return(data.frame(number=dat$number))
 }
}

get.tot.events <- function(stdy,basedata, arm=T){
  if(is.logical(arm)){
    dat <- dplyr::filter(basedata, study==stdy) %>% group_by(Arm)%>%summarise(tot.events=round(esrd*number/100))
    return(dat)
  } else {
    dat <- dplyr::filter(basedata, study==stdy, Arm==arm)%>% summarise(tot.events=round(esrd*number/100))
    return(dat[1,1])
  }
}

get.maxfollowup <- function(stdy, basedata, arm='FullStudy'){
  dat <- basedata
  dat <- dplyr::filter(dat, study==stdy, Arm==arm)
  mx <- unique(dat$maxfollowup/12) # max followup recorded in years
  if(is.na(mx)){
    x = dat[,grep('esrdy[0-9]+$',names(dat))]
    yr = names(x)[max(which(!is.na(x)))]
    yr = as.numeric(gsub('esrdy','',yr))
    mx = yr
  }
  x1 = unique(with(dat,endfu-start))
  x2 = unique(with(dat,pubdate - start -1))
  
  if(!is.na(x1) & mx < x1) mx = x1
  if(!is.na(x2) & mx < x2) mx = x2
  return(mx)
}

get.lag <- function(stdy, basedata,arm='FullStudy'){
  if(is.character(stdy))stdy <- as.numeric(stdy)
  x <- subset(basedata, study %in% stdy & Arm==arm) %>%  select(study, lags, Developed) %>% 
    mutate(lags = ifelse(is.na(lags),0,lags)) %>% select(lags)
  return(x)}

get.sCr <- function(stdy, basedata, arm='FullStudy'){
  if(is.character(stdy)) stdy <- as.numeric(stdy)
  x <- dplyr::filter(basedata, study %in% stdy, Arm==arm) %>% select(study, sCr, Developed)
  return(x)
}
# get.KM <- function(indx, basedata=Summ,datadir='data/fromPapers/'){
#   if(is.character(indx)) indx <- grep(indx, basedata$Study)
#   f <- basedata$File[indx]
#   dat0 <- read.csv(file.path(datadir,f))
#   dat0 <- dat0[!is.na(dat0[,ncol(dat0)]),]
#   n.risk <- get.n.risk(indx, basedata,datadir)
#   dat <- KMclean(dat0,n.risk)
#   return(dat)
# }

# get.at.risk <- function(indx, basedata=Summ,datadir='data/fromPapers/'){
#   if(is.character(indx)) indx <- grep(indx, basedata$Study)
#   n.risk <- get.n.risk(indx, basedata=basedata,datadir)
#   at.risk <- lapply(as.list(n.risk), function(x) data.frame(Year=0, Number.at.risk=x))
#   if(length(at.risk)==1) at.risk <- at.risk[[1]]
#   if(basedata$Details[indx]=='Y'){
#     bl <- read.xlsx('data/Summary2.xls', sheet=basedata$Sheet[indx])
#     if(ncol(bl)==3){
#       bl <- dlply(bl, .(Group), strip_splits)
#       for(u in names(at.risk)) at.risk[[u]] <- rbind(at.risk[[u]], bl[[u]])
#       at.risk <- lapply(at.risk, function(d) d[!duplicated(d),])
#     } else {
#       at.risk <- rbind(at.risk, bl)
#       at.risk <- at.risk[!duplicated(at.risk),]
#     }
#   }
#   return(at.risk)
# }




# Interpolate at.risk times into data, if needed --------------------------

interp <- function(dat, at.risk){
  if(!is.data.frame(dat)){
    out <- list()
    for(u in names(dat)){
      out[[u]] <- interp(dat[[u]],at.risk[[u]])
    }
  } else {
    large.time=100 # to ensure we accomodate at risk times beyond KM
    bl <- cut(at.risk$Year, breaks=sort(unique(c(large.time, dat$Year))), right=FALSE)
    ind <- match(as.character(bl), levels(bl))
    ind <- sort(c(ind, ind+1))
    res <- approx(dat[ind,1],dat[ind,2], at.risk$Year, method='constant')
    res <- as.data.frame(res); names(res)  <- names(dat)
    if(any(is.na(res$Prob))){
      i <- which(is.na(res$Prob))
      res$Prob[i] <- dat$Prob[max(which(dat$Year <= res$Year[i]))]
    }
    out <- rbind(dat, res)
    o <- order(out[,1], -out[,2]) # Ensure probs are descending
    out <- out[o,]
    out <- out[!duplicated(out),]
  }
  return(out)
}

get.at.risk.intervals <- function(dat, at.risk){
#   if(is.data.frame(dat)){
    large.time=100
#     interped <- interp(dat,at.risk)
#     interped <- interped[!duplicated(interped),]
#     print(length(dat$Year))
    ints <- cut(dat$Year, breaks=unique(c(at.risk$Year, large.time)), right=FALSE)
    test_that('',{
      expect_equal(length(ints),nrow(dat))
    })
#   } else {
#     ints <- list()
#     for(u in names(dat)){
#       ints[[u]] <- get.at.risk.intervals(dat[[u]], at.risk[[u]])
#     }
#   }
  return(ints)
}

get.times <- function(dat, n1, n2){
  # n1 = number at risk at beginning of interval
  # n2 = number at risk at end of interval
  # Single arm, single interval
  require(doBy)
  x <- range(dat$Year)
  p <- range(dat$Prob)
  nr = nrow(dat)
  
  # Find number censored
  n.if.no.cens <- round(n1*(p[1]/p[2]))
  n.censored <- ifelse(is.na(n2), 0, n.if.no.cens-n2)
  cens <- d <- jmp <- rep( 0, nrow(dat)-1)
  d.times <- cens.times <- NULL
  n <- n1
  while(n != n2){
    if(n.censored>0){
      cens.times <- x[1]+ppoints(n.censored)*diff(x)
      where <- cut(cens.times, breaks=unique(dat$Year), right=FALSE)
      ind <- lastobs(dat$Year[-which(dat$Year==max(dat$Year))])
      test_that("Finding intervals for cens times",{
        expect_equal(length(ind), length(table(where)))
      })
      cens[ind] <- as.vector(table(where))
    }
    for(i in 2:(nrow(dat))){
      d[i-1] <- round(n*(1-dat$Prob[i]/dat$Prob[i-1]))
      jmp[i-1] <- d[i-1]/n*dat$Prob[i-1]
      n <- n-d[i-1]-cens[i-1]
      print(n)
    }
    n.censored = n.censored + n - n2
    
    print(c(n,n2,n.censored))
  }
  test_that("",{expect_that(length(d)>0, is_true())})

  if(sum(d)>0){
    d.times <- rep(dat$Year[-nr],d)
  }
  return(list(d.times=d.times, cens.times=cens.times))
}
    
get.all.times <- function(dat,at.risk,tot.events){ # single arm
  require(doBy)
  if(!is.data.frame(dat)){
    out <- list()
    for(u in names(dat)){
      out[[u]] <- get.all.times(dat[[u]],at.risk[[u]], tot.events[[u]])
    }
  } else {
    nr <- nrow(at.risk)
    if(nr==1){ # no numbers at risk
      n1 <- at.risk[1,2]; n <- n1
      n.censored = 0
      cens.times <- NULL
      x <- range(dat$Year)
      p <- range(dat$Prob)
      n.if.no.cens <- round(n1*(p[1]/p[2]))
      d <- rep(0,nrow(dat))
      for(i in 2:nrow(dat)){
        d[i] <- as.integer(round(n*(1-dat$Prob[i]/dat$Prob[i-1])))
        test_that("Is d[i] an integer?",{
          expect_equal(d[i]%%1,0)
        })
        n <- n-d[i]
      }
      test_that("integer components",{
        expect_that(n.if.no.cens%%1, equals(0))
        expect_that(n%%1, equals(0))
        expect_equal(length(d),nrow(dat))})
      if(!is.na(tot.events)){
         while(sum(d, na.rm=T)>tot.events){
           
          n <- n1
          n.censored <- min(n1-sum(d),n.censored+sum(d)-tot.events)
          cens <- d <- rep(0, nrow(dat))
          if(n.censored > 0){
#             print(n.censored)
            cens.times <- x[1]+ppoints(n.censored)*diff(x)
            where <- cut(cens.times, breaks=unique(dat$Year), right=FALSE)
            ind <- lastobs(dat$Year[-which(dat$Year==max(dat$Year))])
            #           print(length(ind))
            #           print(length(table(where)))
            test_that("Finding intervals for cens times 2",{
              expect_equal(length(ind), length(table(where)))
            })
            cens[ind] <- as.vector(table(where))
          }
          for(i in 2:(nrow(dat))){
            d[i] <- round(n*(1-dat$Prob[i]/dat$Prob[i-1]))
            n <- n-d[i]-cens[i]
          }
#            print(c(sum(d), n.censored))
          #       }
        }
#         print(sum(table(where)))
#         print(cens)
      }
#       print(d)
      d.times <- rep(dat$Year,d)
      out <- list(d.times=d.times, cens.times=cens.times)
    } else { # Numbers at risk available
    dat1 <- interp(dat, at.risk)
    ints <- get.at.risk.intervals(dat1, at.risk)
#     print(dat1[,1])
#     print(ints)
    test_that("",{
      expect_equal(length(ints),nrow(dat1))
    })
    n1 <- at.risk$Number.at.risk[1:nr]
    n2 <- at.risk$Number.at.risk[2:(nr+1)]
    bl <- split(dat1,ints)
    out0 <- vector('list',nr)
    for(i in 1:(nr-1)) {
#       print(i)
      bl[[i]] <- rbind(bl[[i]],bl[[i+1]][1,])
      bl[[i]] <- bl[[i]][!duplicated(bl[[i]]),]
      out0[[i]] <- get.times(bl[[i]],n1[i], n2[i])
    }
    tot.cens <- do.call(sum, lapply(out0, function(x) length(x$cens.times)))
    tot.deaths <- do.call(sum, lapply(out0, function(x) length(x$d.times)))
    # Last interval ------------------------------------------
    i <- nr
    x <- range(bl[[i]]$Year); p <- range(bl[[i]]$Prob)
    if(nrow(bl[[i]])==1){
      n.censor <- n1[i]
      cens.times <- rep(x[1],n.censor)
      x.thresh=diff(range(dat$Year))/100
      where <- cut(cens.times, breaks=c(x[1],x[1]+x.thresh), right=FALSE)
    } else {
    n.censor <- min(diff(x)/(x[1]-min(dat$Year))*tot.cens, n1[i])
    cens.times <- x[1]+ppoints(n.censor)*diff(x)
    where <- cut(cens.times, breaks=unique(bl[[i]]$Year), right=FALSE)
    }
    cens <- d <- rep(0, nrow(bl[[i]]))
    ind <- lastobs(bl[[i]]$Year)
    cens[ind] <- as.vector(table(where))  
    test_that("",{
      expect_equal(length(ind), length(levels(where)))
    })
    n <- n1[i]
    for(j in 2:(nr)){
      d[j] <- round(n*(1-dat$Prob[j]/dat$Prob[j-1]))
      n <- n-d[j]-cens[j]
    }
    if(!is.na(tot.events)){
      if(tot.deaths>tot.events){
        d.times <- NULL
        cens.times <- NULL
      } else {
      while(tot.deaths+sum(d)>=tot.events | n.censored > 0){
        if(sum(d)+tot.deaths==tot.events) break
        n <- n1
        if(nrow(bl[[i]])==1){
          n.censor <- n1[i]
          cens.times <- rep(x[1],n.censor)
          x.thresh=diff(range(dat$Year))/100
#           print(x.thresh)
          where <- cut(cens.times, breaks=c(x[1],x[1]+x.thresh), right=FALSE)
        } else {
          n.censored <- n.censored + tot.deaths+sum(d)-tot.events
          cens.times <- x[1]+ppoints(n.censored)*diff(x)
          where <- cut(cens.times, breaks=unique(dat$Year), right=FALSE)
        }
        cens <- d <- rep(0, nrow(bl[[i]]))
        ind <- lastobs(bl[[i]]$Year)
        cens[ind] <- as.vector(table(where))  
        if(nrow(bl[[i]])==1){
          d[1] <- 0
          n <- n-d[1] - cens[1]
        } else {
          for(i in 2:(nrow(bl[[i]]))){
            d[i] <- round(n*(1-bl[[i]]$Prob[i]/bl[[i]]$Prob[i-1]))
            n <- n-d[i]-cens[i]
          }
        }
      }
      }
    }
    d.times <- NULL
    test_that('',{expect_that(length(d)>0, is_true())})
    if(sum(d, na.rm=T)>0){
      d.times <- rep(bl[[i]]$Year,d)
    }
    out0[[i]] <- list(d.times=d.times, cens.times=cens.times)
    out <- list(d.times=do.call(c,lapply(out0, function(x) x$d.times)),
                cens.times=do.call(c, lapply(out0,function(x) x$cens.times)))
  }
}
return(out)
}

Get.all.times <- function(indx){
  get.all.times(get.KM(indx),get.at.risk(indx), get.tot.events(indx))
}

gen.data <- function(event.times, n.risk, time.end.of.study){ #event.times = Get.all.times(indx)
  n.events <- sapply(event.times,length)
  n.cens.at.end <- n.risk-sum(n.events)
  newdat <- NULL
  if(n.events['d.times']>0)newdat <- rbind(newdat,cbind(event.times$d.times,rep(1,n.events['d.times'])))
  if(n.events['cens.times']>0){
    newdat <- rbind(newdat,
      cbind(event.times$cens.times, rep(0,n.events['cens.times']))
    )
  }
  if(n.cens.at.end>0){
    newdat <- rbind(newdat, cbind(rep(time.end.of.study,n.cens.at.end),rep(0,n.cens.at.end)))
  }
  return(newdat)
}

pool.ipd <- function(lst){
  d.times <- do.call(c, lapply(lst, function(l) l$d.times))
  cens.times <- do.call(c,lapply(lst, function(l) l$cens.times))
  return(list(d.times=d.times, cens.times=cens.times))
}


get.geog <- function(i, basedata=basedata) basedata$Developed[i]
get.year <- function(i, basedata=basedata) basedata$yearofstudy[i]



#' Function to estimate the number of deaths at a jump of a Kaplan-Meier curve, 
#' and the number of censorings between the current and previous jumps
#' 
#' @param n1 number at risk after previous jump
#' @param p Survival probability after previous jump
#' @param jump size of current jump
#' @return n2 number at risk after current jump
#' @return d number of events at the current jump
#' @return cens number of censored in previous interval
compute <- function(n1,p, jump){
  for(d in 1:n1){
    n2=d*p/jump
    if(round(n2)>n1){
      d <- max(1,d-1)
      break
    }
  } # number of deaths
  n2 <- min(round(d*p/jump),n1)
  if(abs(d*p/n2-jump)>abs(d*p/(n2+1)-jump)) n2 <- min(n1,n2+1)
  cens <- n1-n2 # number of censored
  return(data.frame(n2=n2, d=d, cens=cens))
}
f.n.risk <- function(stdy){
  require(dplyr)
  Summ <- read.xls('data/Summary2.xlsx',sheet='Summary')
  Summ <- Summ[order(Summ$Study),]
  x <- dplyr::filter(Summ, studyno==stdy) %>% select(TotalAtRisk)
  n.risk <- as.numeric(unlist(str_split(x[1,1], ',')))
  dat <- kmdata[[as.character(stdy)]]
  dat <- dat[complete.cases(dat),]
  names(n.risk) = unique(dat$Group)
  return(n.risk)
}
compute.profile <- function(dat, n.risk){
  if(dat[2,1]==dat[1,1]){
    u = diff(range(dat[,1]))/500
    dat[2,1] = u
    dat = rbind(dat[1,], c(u,dat[1,2]), dat[-1,])
  }
  n.risk <- as.numeric(n.risk)
  prob <- round(dat$Prob,2)
  jumps <- -diff(prob) # p0-p1
  ndat <- nrow(dat)
  if(sum(jumps>0)==0){
    d.times <- NULL
    cens.times <- max(dat[,1])*ppoints(n.risk) # Uniform in time
  } else {
    times <- dat[which(jumps>0),1]
    jumps <- jumps[which(jumps>0)]
    # Start at time=0, prob=1
    res <- vector('list',length(jumps)+1)
    n1 <- as.numeric(n.risk)
    p <- 1
    i <- 1
    while(i <= length(jumps)){
      condn <- jumps[i]/p < 1/n1 # Think 1-jumps[i]/p > 1-1/n1
      #print(c(jumps[i]/p, 1/n1, condn))
      if(condn){ # Need to clean to ensure jumps really represent a death given new at-risk numbers 
        d <- KMclean(dat[(2*i-1):nrow(dat),], n1, 
                                      start.time = dat[(2*i-1),1],
                                      start.p = p)
        dat <- rbind(dat[1:(2*i-2),],d)
        dat  <- dat[!duplicated(dat),]
        j <- abs(diff(dat[,2]))
        tt <- dat[j>0,1]
        j <- j[j>0]
        times <- tt
        jumps <- j
      }
      out <- compute(n1,p,jumps[i])
      n1 <- out$n2 - out$d
      p <- p - jumps[i]
      res[[i]] <- out
      if(i == length(jumps)){
        break
      } else {
        i <- i+1
      }
    }
    result <- ldply(res)
#     result <- data.frame(do.call(rbind,res))
    d.times <- rep(times[1:nrow(result)],result$d)
    times <- c(0,times)
    cens.times <- NULL
    for(i in 1:length(jumps)){
      cens.times <- c(cens.times,
                      times[i]+ppoints(result$cens[i])*diff(times[i:(i+1)]))
    }
    # last interval
    cens.times <- c(cens.times,rep(max(dat[,1]),n1))
  }
  return(list(d.times=d.times[d.times>0], cens.times=cens.times))
}

get.IPD <- function(KMdata, n.risk.data){
  if(!is.data.frame(KMdata)){
    l <- list()
    for(i in 1:length(KMdata)){
      l[[i]] <- compute.profile(KMdata[[i]], n.risk.data[i])
      
    }
    names(l) <- names(KMdata)
    out <- l
  } else{
    out <- compute.profile(KMdata,n.risk.data)
  }
  return(out)
}


create.Surv <- function(compute.lst, lag=0){
  if(!('d.times' %in% names(compute.lst))){
    lapply(compute.lst, create.Surv)
  }
  require(survival)
  len <- sapply(compute.lst,length)
  indic <- rep(c(1,0),len)
  compute.lst$d.times[compute.lst$d.times==0] <- 0.0001
  return(Surv(rep(lag,length(indic)),lag+do.call(c,compute.lst),indic))
}

update.compute <- function(compute.lst,dat, at.risk=NULL, tot.events=NULL){
  n.risk <- sum(sapply(compute.lst,length))
  if(!is.null(tot.events)){
    jmp.times <- unique(compute.lst$d.times)
    jumps <- abs(diff(dat[,2])); jumps <- jumps[jumps>0]
    cens.times <- compute.lst$cens.times
    if(length(compute.lst$d.times)>tot.events){
      new.tot.events <- length(compute.lst$d.times)
      while(new.tot.events > tot.events){
        add.cens <- new.tot.events-tot.events
        # where do you add the censoring?
        km.intervals <- cut(cens.times, c(0,jmp.times),right=T, include.lowest=T)
        freq <- table(as.numeric(km.intervals))
        pr <- freq/sum(freq)
        cens.new <- round(pr*add.cens)
        jmp.intervals <- c(0,sort(jmp.times))
        cens.new.times <- NULL
        for(i in names(cens.new)){
          ind <- as.numeric(i)
          cens.new.times <- c(cens.new.times, 
                              jmp.intervals[ind]+ppoints(cens.new[i])*diff(jmp.intervals[ind:(ind+1)]))
        }
        cens.times <- sort(c(cens.times, cens.new.times))
        # Put censored where it can make a difference.
        # Now recompute deaths
        n1 <- n.risk
        p <- 1
        d <- rep(0,length(jmp.times))
        freq <- table(cut(cens.times, jmp.intervals, right=T, include.lowest=T))
        for(i in 1:length(jmp.times)){
          n1 <- n1-as.numeric(freq[i])
          bl <- compute(n1,p,jumps[i])
          d[i] <- bl$d
          p <- p-jumps[i]
          n1 <- n1-d[i]
        }
        n.events <- sum(d)
        n.cens <- n.risk-n.events
        if(n.cens < length(cens.times)) cens.times <- cens.times[1:n.cens]  
        if(n.events==new.tot.events) break
        new.tot.events <- n.events
      }
      d.times <- rep(jmp.times, d)
      
    }
  }
  return(list(d.times=d.times, cens.times=cens.times))
}

survtime.ci <- function(params,times=c(5,10,15)){
  # Survival time confidence intervals based on estimates of 
  # Weibull parameters from phreg
  require(mvtnorm)
  ests <- pweibull(times, scale=exp(params$coef[1]), shape=exp(params$coef[2]), lower.tail=F)
  ests <- data.frame(t(ests));names(ests) <- paste('yr',times,sep='')
  set.seed(15345)
  x <- rmvnorm(1000, mean=params$coef, sigma=params$vars)
  x2 <- exp(x);colnames(x2) <- c('scale','shape')
  bl <- data.frame(do.call(cbind,lapply(times, pweibull, scale=x2[,1], shape=x2[,2], lower.tail=F)))
  names(bl) <- paste('yr',times,sep='')
  out <- rbind(ests,sapply(bl,quantile, c(0.025,0.975)))
  row.names(out)[1] <- 'Estimate'
  return(out)
}

#################################################
# Functions for summary survival data -------------------------------------
fn.env <- new.env()
source('lib/truncFunctions.R', local=fn.env)


## Determining the appropriate weibull parameters
weib.param <- function(summData, Lag=0){
  yr <- summData$Year; p <- summData$Prob
  if(length(yr)==1){
    yr <- c(0.1,yr); p <- c(0.999,p)
  }
  p = pmax(0.001, pmin(0.999,p))
  if(Lag==0){ # Use Weibull plot method
    #ind <- (p>0 & p<1);yr <- yr[ind];p <- p[ind]
    m <- lm(log(-log(p))~log(yr))
    k <- m$coef[2]
    lambda <- exp(-m$coef[1]/k)
  } else { # Use optimization
    obj <- function(params){sum((log(1-p) - log(ptrunc(as.numeric(yr+Lag),'weibull',a = as.numeric(Lag),shape=params[1],scale=params[2])))^2)}
    ests <- optim(c(1.5,1.5),obj, control=list( maxit=2000))
    k <- ests$par[1]
    lambda <- ests$par[2]
  }
  return(data.frame(shape=k,scale=lambda))
}

find.cens <- function(x,n,target, a,b){
  j=0
  p=prod(1-1/rev((n-length(x)+1):n))
  while(round(p,2) >= target & j <n-length(x)){
    j=j+1
    y = a + ppoints(j)*(b-a)
    o = order(c(x,y))
    st = c(rep(1,length(x)),rep(0,length(y)))[o]
    p = prod(1 - st/(n-(1:length(st))+1))
#     print(p)
  }
  return(max(0,j-1))
}

# compute.jump <- function(n.risk, events){
#   n.events <- length(events)
#   sum(events/(n.risk-(1:n.events)+1))
# }

# summ2IPD <- function(summData, params, n.risk, n.events,maxfollowup, lags=0){
#   set.seed(980)
#   # Fix Weibull parameters
#   k <- params$shape
#   lambda <- params$scale
#   # Grab summary data
#   yr <- summData$Year
#   surv <- summData$Prob
#   times <- c(0,yr,maxfollowup)
#   surv <- c(1,surv, NA)
#   d <- diff(surv)
#   ind <- which(abs(d)==0) # ensure at least 1 event between intervals
#   if(length(ind)>0){
#     times <- times[-(ind+1)]
#     surv <- surv[-(ind+1)]
#   }
#   n <- n.risk
#   events <- 0
#   censoreds <- 0
#   jumps <- abs(diff(surv)) # Jumps in the survival curve
#   times.events <- times.cens <- NULL
#   for(i in 1:(length(jumps)-1)){
#     j <- 0; e <- 0
#     while(j < jumps[i]){
#       e <- e+1
#       if(events+e > n.events - length(jumps)+i+1) break # too many events, taking into account events in next intervals
#       j <- compute.jump(n, rep(1,e))
#     }
#     e <- max(1,e-1)
#     events <- events+e # at least 1 event in interval
#     tm <- rtrunc(e,'weibull',shape=k, scale=lambda,a=lags+times[i], b=lags+times[i+1])-lags
#     t.events <- sort(tm) #event times
#     cens <- 0
#     j <- compute.jump(n, rep(1,e))
#     n.cens=0
#     t.cens <- NULL
#     while(j < jumps[i]){
#       n.cens <- n.cens+1
#       if(censoreds + n.cens > n.risk - n.events){
#         n.cens=n.cens-1
#         break
#       }
#       t.cens <- times[i]+ (1:n.cens/(n.cens+1))*(times[i+1]-times[i]) # Censored times
#       bl <- c(t.events,t.cens)
#       ind <- c(rep(1,length(t.events)), rep(0, length(t.cens)))
#       ind <- ind[order(bl)]
#       j <- compute.jump(n, ind)
#       n.cens <- length(t.cens)
#     }
#     n.cens <- max(0,n.cens-1)
#     censoreds <- censoreds+max(0,n.cens)
#     times.events <- c(times.events, t.events)
#     if(n.cens>0)t.cens <- times[i]+(1:n.cens/(n.cens+1))*(times[i+1]-times[i])
#     times.cens <- c(times.cens, t.cens)
#     ind <- c(rep(1,length(t.events)),rep(0,length(t.cens)))
#     bl <- c(t.events, t.cens)
#     ind <- ind[order(bl)]
#     j1 <- compute.jump(n,ind)
#     n <- n - length(t.events)-length(t.cens)
# #     print(c(i, events, n.cens,j1, jumps[i]))
#   }
#   if(events<n.events){
# #     print(c(lags,times[i+1],maxfollowup))
#     tm <- rtrunc(n.events - events, 'weibull',shape=k, scale=lambda, a = lags+times[i+1], b= lags+maxfollowup)-lags
# #     print(tm)
#     times.events <- c(times.events,tm)
#     n <- n-length(tm)
#   }
#   if(n > 0){
#     times.cens <- c(times.cens,
#                     times[i+1]+(maxfollowup-times[i+1])*(1:n/(n+1)))
#   }
#   return(list(d.times=sort(times.events),cens.times=sort(times.cens)))
# }

summ2IPD <- function(summData, params, n.risk, n.events,maxfollowup, lags=0){
  set.seed(1000)
  k <- params$shape
  lambda <- params$scale
  yr <- summData$Year
  surv <- summData$Prob
  if(length(lags)>1){lags = lags$lags}
  times = c(0,yr, maxfollowup)
  surv = c(1,surv, NA)
  jmps <- round(exp(diff(log(surv))),2) # Proportional jump between times (just like KM)
#   ind1 <- which(jmps==1 & surv[-length(surv)]==1) # No change
#   ind2 <- which(jmps==1 & surv[-length(surv)]!=1)
#   if(length(ind1)>0){
#     times  <- times[-(ind1)]
#     surv <- surv[-(ind1)]
#   }
#   if(length(ind2)>0){
#     times <- times[-(ind2+1)]
#     surv <- surv[-(ind2+1)]
#   }
#   jmps <- round(exp(diff(log(surv))),2)
  n <- as.numeric(n.risk)
  times.d <- times.cens <- numeric(0)
  if(all(surv[-length(surv)]==1)){
    times.d = numeric(0)
    times.c = 0+ppoints(n)*maxfollowup
    return(list(d.times=times.d, cens.times=times.c))
  }
  for(i in 1:length(jmps)){
    last.interval <- is.na(surv[i+1])
    if(!last.interval){
      if(jmps[i]==1) next # Assumes no censoring either
      n.d <- sum(cumprod(1-1/rev(1:n))>jmps[i])
      if(n.d==0) n.d <- 1
      n.d <- min(n.d, n.events-length(times.d)-(length(jmps)-i-1)) # Account for events in other jumps. Forces compensation by censoring
      x <- rtrunc(n.d, 'weibull',scale=lambda, shape=k, 
                  a=as.numeric(times[i]+lags), b=as.numeric(times[i+1]+lags))-as.numeric(lags)
      n.c <- find.cens(x,n,jmps[i],times[i], times[i+1])
      n.c <- min(n.c, n - n.d)
      y <- times[i]+ppoints(n.c)*(times[i+1]-times[i])
      times.d <- c(times.d,x)
      times.cens <- c(times.cens,y)
      n <- n-length(c(x,y))
    } else {
      if(times[i]==times[i+1]) times[i+1] <- times[i]+5
      n.d <- n.events - length(times.d)
      sn.c <- n - n.d
      x = rtrunc(n.d, 'weibull',scale=lambda, shape=k, a=times[i], b=times[i+1])
      y = times[i]+ppoints(sn.c)*(times[i+1]-times[i])
      times.d <- c(times.d,x)
      times.cens <- c(times.cens,y)
    }
  }
  return(list(d.times=times.d, cens.times=times.cens))
}

fnlist = list(n.risk=get.n.risk, n.events=get.tot.events, maxfollowup=get.maxfollowup, lags=get.lag)

getIPD.summ <- function(summData){
  stdy <- unique(summData$study)
  arm <- unique(summData$Arm)
  lags <- get.lag(stdy, basedata, arm=arm)
  arg <- list(summData=summData, params=weib.param(summData, Lag=lags))
  arg <- c(arg, lapply(fnlist, do.call, list(stdy=stdy, basedata=basedata, arm=arm)))
  if(any(is.na(arg))){
    return(NULL)
  } else {
    return(do.call(summ2IPD, arg))
  }
}

IPD <- function(x){
  # Take results of summ2IPD or get.IPD, convert to survival object
  require(survival)
  s <- Surv(do.call(c,x), rep(c(1,0), sapply(x,length)))
  return(s)
}

######################################################################################
plot1 <- function(dat){
  plot(dat, type='l', ylim=c(0,1))
}

unduplicate <- function(x) sapply(strsplit(x,','), function(m) paste(unique(m), collapse=','))



# Computing analytic start date for each study ----------------------------
#' Compute year the study begins
#' 
#' Compute the start year of each study, in case it is missing
beginYear <- function(dat=basedata){
  starts <- dat$start
  ind <- which(is.na(starts))
  starts[ind] <- with(dat[ind,], round(pubdate - pmax(maxfollowup, esrdfu..m.,na.rm=T)/12-1))
  ind <- which(is.na(starts)) # just arms of 118
  starts[ind] <- starts[min(ind)-1]
  return(starts)
}

#' Compute analytic start year
#'
#' Compute the analytic start year for each study as a proportion of the study duration
#' @param dat=basedata data.frame containing start and end information for each study
#' @param  prop=0.5 Proportion into study when analytic start is (0.75=3/4 into study)
#' @return A vector giving the analytic start years of each study
startYear <- function(dat=basedata, prop=0.5){
  require(dplyr)
  timedat <- select(dat, start, endfu, endenroll)
  yr_of_study <- round(with(timedat, (start+(pmin(endfu, endenroll, na.rm=T)-start)*prop))) #counting from start
  ind <- which(is.na(yr_of_study))
  yr_of_study[ind] <- ifelse(is.na(timedat$start[ind]),NA, round(timedat$start[ind]+(dat$pubdate[ind]-1-timedat$start[ind])*prop)) #counting from start
  ind <- which(is.na(yr_of_study))
  yr_of_study[ind] <- round(pmin(dat$endfu[ind], dat$pubdate[ind]-1, na.rm=T) - pmax(dat$esrdfu..m.[ind], dat$maxfollowup[ind], na.rm=T)/12*(1-prop)) #counting from end
  ind <- is.na(yr_of_study)
  yr_of_study[ind] <- rep(yr_of_study[dat$study==118 & !ind], sum(ind))
  return(yr_of_study)
}

startYear2 <- function(dat=basedata, prop=0.75){
  yr_of_study <- startYear(dat=dat, prop=prop)
  enddate <- pmax(dat$endfu, dat$pubdate-1, na.rm=T)
  ind <- which(enddate > dat$endenroll)
  yr_of_study[ind] <- dat$endenroll[ind]
  return(yr_of_study)
}

#' Compute the analytic end year 
#'
#' The analytic end year of the study is the end of the period that will get credit in Moving Average analysis
#' It defaults to the end of the study, defined as the last of the end of followup, end of enrollment or one year
#' before publication. 
#' @param dat=basedata data.frame containing start and end information from each study
#' @param maxduration=NULL The maximum number of years a study can get credit
#' @param ... Additional parameters sent to startYear
#' @return A vector giving the analytic end years of each study
#' @export
#' @keywords
#' @seealso
#' @alias
#' @examples
endYear <- function(dat=basedata, maxduration=NULL,...){
  end <- pmax(dat$endfu, dat$endenroll, dat$pubdate-1, na.rm=T)
  if(!is.null(maxduration)){
    start <- startYear(dat, ...)
    end <- pmin(end, start+maxduration)
  }
  return(end)
}
# Function to generate data subsets for continuous analysis --------------------

#' Creating windows for Moving Average analysis
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#'
#' @description
#' This function inputs start dates and widths of each temporal window and 
#' returns a data.frame with the years in each window
#' @param wins=1964:2008 Start years of each window
#' @param  width=5 Width of windows in years
#' @return A data.frame with years included in each window
createWindows <- function(wins=1964:2008, width=5){
  x <- matrix(0,ncol=width, nrow=1)
  for(i in wins){
    x <- rbind(x, seq(i, i+width-1))
  }
  x <- x[-1,]
  return(x)
}


#' @title Identifying study membership in each window
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function inputs the start and end dates of each study and determines 
#' which studies overlap with each window in the Moving Average analysis. It
#' returns, for each window, the study IDs for studies that are members of that window
#' @param start_date A vector of start dates for each study
#' @param  end_date A vector of end dates for each study
#' @param  studynames=NULL Study IDs
#' @param  wins=1964:2008 Start dates of windows
#' @param  winwidth=5 Width of each window in years
#' @return A list of studies overlapping with each window
#' @examples
#' require(dplyr)
#' x1 <- basedata %>% mutate(start_date=yr_of_study, end_date=pmax(endfu, endenroll, pubdate-1, na.rm=T)) %>% 
#'    select(study, Arm, start_date, end_date, Group_class)  %>% 
#'    dplyr::filter(Arm=='FullStudy')
#' members <- windowMembership(x1$start_date, x1$end_date, x1$study)
windowMembership <- function(start_date, end_date, studynames=NULL, wins=1964:2008, winwidth=5){
  if(any(start_date>end_date)){
    stop('Invalid dates')
  }
  if(length(start_date)!=length(end_date)){
    stop('Lengths of dates are incompatible')
  }

  # Create windows for moving average
  Windows <- createWindows(wins=wins, width=winwidth)

  # Find years covered by each study
  coverage <- vector('list', length(start_date))
  for(i in 1:length(start_date)){
    coverage[[i]] <- seq(start_date[i],end_date[i])
  }
  names(coverage) <- studynames

  # Book keeping which study is part of which window
  books <- matrix(0, ncol=length(coverage), nrow=nrow(Windows))
  for(j in 1:length(coverage)){
    for(i in 1:nrow(Windows)){
      books[i,j] <- length(intersect(coverage[[j]], Windows[i,]))>0
    }
  }
  books <- as.data.frame(books)
  names(books) <- names(coverage)

  # Find which studies are members of which window
  members <- vector("list", nrow(books))
  for(i in 1:nrow(books)){
    members[[i]] <- unique(names(books)[books[i,]==1])
  }
  return(members)
}

# Creating JAGS-compatible data -------------------------------------------


#' Converting a list of IPD into JAGS-compatible data
#' 
#' @param ipd The list of IPDs
#' @param info Other data, matched on some index with the names of the IPD list, that will be included
#'            in the JAGS dataset
gen.jagsdata <- function(ipd, info){
  data.jags <- list(td=NULL, tcens=NULL, trunc=NULL, isCensored=NULL, geog=NULL, yr = NULL)
  data.jags <- data.frame(data.jags)
  n <- length(ipd)
  for(i in 1:n){
    dat = ipd[[i]]
    studyno= as.numeric(names(ipd))[i]
    study.info <- subset(info, study==studyno)
    t.d <- c(dat$d.times, rep(NA,length(dat$cens.times)))
    trunc <- rep(ifelse(is.na(study.info$lags), 0, study.info$lags), length(t.d))
    t.cens = c(rep(1000, length(dat$d.times)), dat$cens.times) # Need 1000 to be bigger than any times in ipd
    t.d <- t.d + trunc
    t.cens <- t.cens+trunc
    is.censored <- as.integer(is.na(t.d))
    geog = rep(study.info$Developed, length(t.d))
    yr <- rep(study.info$Decades, length(t.d))
    bl <- list(td=t.d, tcens=t.cens, trunc=trunc, isCensored=is.censored, geog=geog, yr=yr)
    data.jags <- rbind(data.jags, bl)
  }
  return(as.list(data.jags))
}

#' Creating data for import into JAGS
#' 
#' This function inputs data from the survival studies, including IPD data and 
#' followup data, and converts it into the R dump() format for import into
#' JAGS 
#' 
#' @param ipd This is a list of IPD data, one per study. Each element is
#'            itself a list comprising 2 elements, t.d (death) and t.cens (censored) times
#' 
#' @param  follow A data.frame that contains information from studies that 
#'                contains only followup time and number of events, in addition
#'                to some study characteristics
#' @param  info A data.frame that contains information about each study
#' @return A list in R dump() format using all the input data that is amenable for
#'         processing by JAGS
#' 
datForJags <- function(ipd, follow, info){
  out <- follow[,c('number','maxfollowup','Events','Developed',
                   'lags')]
  dat.jags <- as.data.frame(gen.jagsdata(ipd,info))
  names(out)[c(1,4)] <- c('n','geog2')
  out$isCensored2 <- as.integer(rep(1,nrow(out)))
  out$Y <- rep(NA, nrow(out))
#   out$yr2 <- as.factor(out$yr2)
  out$geog2 <- as.factor(out$geog2)
  
  dat <- as.list(out)
  dat$N2 <- nrow(out)
  names(dat.jags) <- c('td','tcens','trunc','isCensored1','geog1','yr1')
  dat.jags <- dat.jags[,-6]
#   dat.jags$yr1 <- as.factor(dat.jags$yr1)
  dat.jags$geog1 <- as.factor(dat.jags$geog1)
  
  dat <- c(dat, as.list(dat.jags))
  dat$N1 <- nrow(dat.jags)
  for(i in 1:length(dat)){
    if(is.factor(dat[[i]])) dat[[i]] <- as.numeric(dat[[i]])
  }
  dat$J <- length(unique(c(dat$geog1,dat$geog2)))
#   dat$K <- length(unique(c(dat$yr1,dat$yr2)))
  return(dat)
}

#' @title Create datasets in a folder that are required for the Moving Average estimation
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' 
#' @description
#' This function creates JAGS-compatible datasets and adds required files to a new folder for a particular
#' Moving Average analysis based on annual windows. It adds IPD data from the studies which are members 
#' of each window into a JAGS-compatible file.
#' 
#' @param members A list of which studies are members in each temporal window
#' @param  outdir Directory to store the files
#' @param  info Study information (including studi ID, class, developed status, decade of study and left truncation info)
#' @param  minkm=5 Minimum number of Kaplan-Meier studies to be included per window
#' @param ipd=study.ipd A list of study-specific IPD, derived from KM curves
#' @param  followup=followup.fullstudy data.frame with followup information (max followup, events, number at risk, etc)
createDatasets <- function(members, outdir, info, minkm=5,ipd=study.ipd, followup=followup.fullstudy){
  require(dplyr)
  outdir <- file.path('data','mcmc',outdir)
  if(file.exists(outdir)) unlink(outdir, recursive=T)
  dir.create(outdir, showWarnings=FALSE) # ensure directory exists
  for(i in 1:length(members)){
    print(i)
    ind1 <- intersect(members[[i]],names(ipd))
    if(length(ind1)< minkm) next # need at least minkm KM curves present in window
    ind2 <- intersect(as.numeric(members[[i]]), followup$study)
    ipd1 <- ipd[ind1]
    follow <- dplyr::filter(followup, study%in% ind2)
    jagsdata <- datForJags(ipd1,follow,info)
    if(!is.null(jagsdata)){
      assign(paste('data.mixed',i,sep=''), jagsdata)
      dump(paste('data.mixed',i,sep=''), file=file.path(outdir,paste('fullmodel.mixed',i,'.txt',sep='')))
    }
  }
  file.copy('fullmodelcts.bug',outdir)
  file.copy('Rfile.R', outdir)
  file.copy('scripts/template.py',outdir)
}

#' Creating Lupus class-specific datasets for Moving Average analysis
#'
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function imports IPD information and study information and creates 
#' JAGS-compatible data sets for each window of the Moving Average analysis, saving them into a specified
#' directory
#' @param outdir Directory where data will be stored
#' @param  study.ipd A list with available IPD for the particular Lupus class
#' @param  classdata Basic study information for the studies
#' @param  classfollow Followup information for the studies
#' @param  members List of which studies are members of which windows (output of \code{\link{windowMembership}})
#' @param  minkm=3 Minimum number of KM studies required in each window
createClassData <- function(outdir, study.ipd, classdata, classfollow,members=members,  minkm=3){
  classnames <- as.character(c(names(study.ipd),names(classfollow)))
  info <- merge(classdata, select(basedata,study, Arm, lags), by=c('study','Arm'))
  classmembers  <- lapply(members, intersect, classnames)
  classfollow2 <- ldply(classfollow); names(classfollow2)[1] <- 'study'
  classfollow2 <- merge(classfollow2, select(basedata, study,Arm, number, 
                                             Developed, Decades, lags),
                        by=c('study','Arm'))
  createDatasets(classmembers, outdir, info, minkm, ipd=study.ipd, followup=classfollow2)
}

# Extracting the results from Moving Average MCMC -------------------------

#' Extracting summaries from MCMC output
#'
#' This function looks through a directory of MCMC output for the Moving Average analysis,
#' parses the .rda files for each window, and compiles posterior median and 95% credible intervals 
#' for 5 year, 10 year and 15 year survival, stratified by developed and developing countries.
#' 
#' @param resdir The directory where the .rda files reside
#' @param  basedir=file.path('data', 'mcmc') The base path to the mcmc results files
#' @return a list of lists, first by year and then by development status
#' @export
#' @example
#' outMixed <- mcmcout('FullMixed')
mcmcout <- function(resdir, basedir=file.path('data','mcmc')){
  require(coda)
  outcts <- vector('list',3)
  for(i in 1:3) outcts[[i]] <- vector('list',2)
  for(i in 1:3){
    for(j in 1:2){
      outcts[[i]][[j]] <- matrix(NA, ncol=3, nrow=48)
    }
  }
  names(outcts) <- c('pr5','pr10','pr15')
  
  for(i in 1:48){
    print(i)
    datfile = file.path(basedir,resdir,paste('Rfile',i,'.rda',sep=''))
    if(!file.exists(datfile)) next
    load(datfile)
    s <- summary(codaSamples)
    quants <- s$quantiles
    if(length(grep('\\[',row.names(quants)))==0){
      for(u in paste('pr',c(5,10,15),sep='')){
        outcts[[u]][[1]][i,] <- quants[u,c(1,3,5)]}
    } else {
      x <- row.names(quants)
      x <- grep('pr',x,value=T)
      nms = do.call(rbind,strsplit(gsub('\\]','',x),'\\['))
      for(k in 1:nrow(nms)){
        if(quants[x[k],3]==0)next
        outcts[[nms[k,1]]][[as.numeric(nms[k,2])]][i,] <- quants[x[k],c(1,3,5)]
      }
    }
  }
  return(outcts)
}

#' Collapsing the summary MCMC data into a single data.frame
#'
#' This function inputs a hierarchical list of summary data (by strata) and
#' creates a single \code{data.frame} with columns specifying 
#' each stratum.
#'
#' @param out The output from \code{\link{mcmcout}}, the results from parsing analyses from a 
#'   single directory, representing a single analysis
#' @param windowctr The center year of each time window in the analysis
#' @examples
#' outMixed <- mcmcout('FullMixed')
#' resultsMixed <- collapseResults(outMixed)
collapseResults <- function(out, windowctr = Windows[,3]){
  require(plyr)
  if(packageVersion('plyr') < '1.8.1') stop('Need plyr version 1.8.1 or greater') # for ldply
  for(i in 1:3){
    for(j in 1:2){
      out[[i]][[j]] <- as.data.frame(out[[i]][[j]])
      out[[i]][[j]]$yr <- windowctr
      names(out[[i]][[j]]) <- c('LB','Med','UB','yr')
    }
    names(out[[i]])=c('Developed','Developing')
  }
  bl=ldply(
    lapply(out, ldply, 
           .id='Dev'),
    .id='Year')
  levels(bl$Year) <- c('5 years','10 years','15 years')
  return(bl)
}

#' @title Extract a credible interval from a single analytic window
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function extracts a CR from the simulated posterior distribution that is the outcome of 
#' an MCMC run.
#'
#' @param yr The center year of the window you want to query
#' @param  project='FullMixed' The study we want to query. This will be matched to directory names
#' @param  parameter='all' Specification of parameters we want to query. Default is all output parameters (not used)
#' @param  level=0.95 Probablity level (not used)
#' @param  digits Number of digits to report
#' @return A data.frame object with stratum information, posterior median and credible intervals
#' 
singleCR <- function(yr, project='FullMixed', parameter='all',level=0.95, digits=3) {
  # Packages
  require(coda)
  require(stringr)
  # Sanity checks
  if(yr < 1966 | yr>2010){stop('Year is out of range')}
  if(!(project %in% dir('data/mcmc'))){stop('Project does not exist')}
  if(level < 0 | level > 1){stop('level not in (0,1)')}
  rdafile <- file.path('data','mcmc',project,paste('Rfile',yr-1965,'.rda',sep=''))
  if(!file.exists(rdafile))stop(paste('No data is available for the year',yr))
  # Create separate environment for loading
  env <- new.env()
  # Extract data
  load(rdafile, envir=env)
  x <- summary(env$codaSamples, quantiles=c(0.5, (1-level)/2, 1-((1-level)/2)))$quantiles
  x[which(x[,1]==0),] <- NA
  out <- data.frame(x[grep('pr',row.names(x), value=T),])
  if(length(grep('\\[[1-2]\\]',row.names(out)))==0) row.names(out) <- paste(row.names(out),'[1]',sep='')
  x <- do.call(rbind,str_extract_all(row.names(out),'[0-9]+'))
  out <- cbind( Developed = ifelse(x[,2]=='1','Developed','Developing'),Years = x[,1], out)
  out <- out[order(out$Developed, as.numeric(out$Years)),]
  colnames(out)[-(1:2)] <- c('Median',paste('LCB (',level,')',sep=''),
                             paste('UCB (',level,')',sep=''))
  out[,c(3,4,5)] <- signif(100*(1-out[,c(3,5,4)]),digits)
  return(out)
}


#' @title Credible intervals from data covering a temporal interval
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @description
#' This function inputs a temporal interval, identifies all studies that contribute
#' to that interval, pools the data and uses JAGS to obtain the posterior sampled
#' distribution of the survival parameters. It then extracts the posterior median and posterior
#' credible intervals for each parameter
#' 
#' It is assumed that there are separate priors for Developed and Developing countries,
#' but the prior is the same across the years in the interval
#' @param startyr Start year of interval
#' @param  endyr End year of interval
#' @param  project='FullMixed' Currently implemented for {FullMixed, Class3, Class4, Class5}
#' @param  level=0.95 Coverage probability of the interval
pooledCR <- function(startyr, endyr, project='FullMixed', level=0.95, digits=3){
  # Packages
  require(rjags)
  require(stringr)
  
  # Sanity checks
  if(startyr < 1966 | endyr > 2010) stop('Years out of range')
  if(startyr > endyr) stop('Need the start year to be before the end year')
  if(!(project %in% c('FullMixed','Class3','Class4','Class5'))){
    stop(paste('Cannot obtain data for ',project))
  }
  if(level<0|level>1) stop('level is not in (0,1)')
  
  # Acquire temporal metadata
  env <- new.env()
  load('data/rda/bookkeeping.rda',envir=env)
  
  # Identify studies
  yrs <- seq(startyr, endyr, by=1)
  x <- sapply(lapply(env$coverage, intersect, yrs), any)
  studies <- names(x)[x]
  
  # Acquire study metadata
  load(file.path('data','rda','WardTableFinal.rda'), envir=env)
  
  # Acquire study data
  if(length(grep('Class',project))>0){
    proj <- tolower(project)
    load(file.path('data','rda',paste(proj,'mcmc.rda',sep='')), envir=env)
    ipd <- env$study.ipd[names(env$study.ipd) %in% as.character(studies)]
    classdata <- get(paste(proj,'data',sep=''), envir=env)
    info <- merge(classdata, select(env$basedata,study, Arm, lags), 
                  by=c('study','Arm'))
    classfollow <- get(paste(proj,'follow',sep=''), envir=env)
    follow <- ldply(classfollow); names(follow)[1] <- 'study'
    follow <- merge(follow, select(env$basedata, study,Arm, number, 
                                   Developed, Decades, lags),
                    by=c('study','Arm'))
    follow <- dplyr::filter(follow, study %in% as.numeric(studies))
  } else {
    load(file.path('data','rda','mixedmcmc.rda'), envir=env)
    ipd <- c(env$keep.overall, env$keep.summ)
    ipd <- ipd[names(ipd) %in% as.character(studies)]
    info <- dplyr::filter(env$basedata, study %in% as.numeric(studies), 
                          Arm=='FullStudy', Group_class=='Mixed') %>%
      select(Author, study, Group_class, Developed, Decades, lags)
    follow <- dplyr::filter(env$followup.fullstudy, study %in% as.numeric(studies))
  }
  d <- datForJags(ipd, follow, info)
  codaSamples <- runjags(d)
  quant <- summary(codaSamples, quantiles=c(0.5, 0.5-level/2, 0.5+level/2))$quantile
  quant <- data.frame(quant[grep('pr',row.names(quant)),])
  if(length(grep('\\[',row.names(quant)))==0){
    row.names(quant) <- paste(row.names(quant),'[1]',sep='')
  }
  teststring <- 'pr([0-9]+)\\[([1-2])\\]'
  quant <- cbind('Developed'=ifelse(gsub(teststring,'\\2',row.names(quant))=='1',
                                    'Developed','Developing'),
                 Year = as.numeric(gsub(teststring,'\\1',row.names(quant))), quant)
  quant <- quant[order(quant$Developed, quant$Year),]
  colnames(quant)[-(1:2)] <- c('Median',
                               paste('LCB (', level,')', sep=''),
                               paste('UCB (', level,')', sep=''))
  quant[,c(3,4,5)] <- signif(100*(1-quant[,c(3,5,4)]),digits)
  return(quant)
}


# Running JAGS -----------------------------------------------------------------

#' Send data to JAGS
#'
#' Given a data set that is formated for ingestion into JAGS, e.g., the 
#' output from datForJags, this function wraps rjags and sets some defaults for 
#' parameters to be sent to rjags. The JAGS files are hardcoded here for the 
#' particular application we are considering here, but this can certainly be
#' adapted.
#' @param dat Data in the form of a list, conforming to the JAGS program being run.
#' This list is essentially in the form of the output from a dump() statement
#' @param  nadapt=1000 Number of adaptation iterations in JAGS
#' @param  niter=1000 Number of interations of JAGS
#' @param  nchains=1 Number of chains to run
#' @param  thin=1 How much to thin the chains by
#' @return A mcmc.list object giving the results of the JAGS run. This essentially
#' gives the sampled posterior distribution for each parameter of interest.
runjags <- function(dat, nadapt=1000, niter=1000, nchains=1, thin=1){
  require(rjags)
  load.module('glm')
  load.module('lecuyer')
  # source('fullmodel.mixed.txt')
  if(dat$N2 > 0){
    inits <- with(dat, 
                  list(lambda = rep(1,J),
                       nu = rep(1, J),
                       .RNG.name = "lecuyer::RngStream",
                       .RNG.seed = sample(1:10000,1),
                       td = ifelse(isCensored1==1, 100, NA), # init for censored td
                       Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                       #                      ypred14 = 100,
                       #                      ypred24 = 100
                  )
    )
    
    parameters <- c('lambda','nu','pr5','pr10','pr15')
    
    mod <- jags.model("fullmodelcts.bug",
                      data = dat,
                      inits = inits,
                      n.chains = nchains, # Change to 4 after testing
                      n.adapt = nadapt)
    #update(mod, n.iter=1000) # Burn-in
    codaSamples <- coda.samples(mod, variable.names=parameters, 
      n.iter=niter, thin=thin)
  } else {
    inits <- with(dat, 
                  list(lambda = rep(1,J),
                       nu = rep(1, J),
                       .RNG.name = "lecuyer::RngStream",
                       .RNG.seed = sample(1:10000,1),
                       td = ifelse(isCensored1==1, 100, NA) # init for censored td
                       #Y = ifelse(isCensored2==1, n, NA)# init for censored Y
                       #                      ypred14 = 100,
                       #                      ypred24 = 100
                  )
    )
    
    parameters <- c('lambda','nu','pr5','pr10','pr15')
    
    mod <- jags.model("fullmodelcts2.bug",
                      data = dat,
                      inits = inits,
                      n.chains = nchains, # Change to 4 after testing
                      n.adapt = nadapt)
    #update(mod, n.iter=1000) # Burn-in
    codaSamples <- coda.samples(mod, variable.names=parameters, 
      n.iter=niter, thin=thin)
  }
  return(codaSamples)
}


# Visualizations ----------------------------------------------------------


#' Plotting the results of continuous analysis, with panels by Developed status
#'
#' This function takes the results of \code{\link{collapseResults}} and plots them, with colors
#' representing incidence rates after 5, 10 and 15 years, dotted lines representing a 95% posterior
#' interval, and panels for Developed and Developing countries
#'
#' @param bl The results from collapseResults
#' @import ggplot2
#' @examples
#' outMixed <- mcmcout('FullMixed')
#' resultsMixed <- collapseResults(outMixed)
#' plt <- pltResults(resultsMixed) + ggtitle('Mixed studies')
pltResults <- function(bl){
  require(ggplot2)
  levels(bl$Dev) <- paste(levels(bl$Dev),'countries')
  levels(bl$Year) <- gsub('s','',levels(bl$Year))
  p=ggplot(bl,aes(x=yr,y=1-Med, group=Year,color=Year))+geom_line(size=1)+
    geom_line(aes(x=yr, y=1-LB),linetype=2)+geom_line(aes(x=yr, y=1-UB),linetype=2)+
    facet_wrap(~Dev, ncol=1)+ylim(0,1)+scale_color_hue(l=40)+
    labs(x='Year',y='Probability of developing ESRD',color='')+
    theme_bw()+theme(legend.key=element_blank(), legend.position=c(0.8,0.9),
                     legend.background=element_rect(fill="transparent"))+xlim(1970,2010)
  return(p)
}

#' Extracting data for staircase plot
#' 
#' This function takes the baseline study information and the study arm and extracts information needed
#' for creating a \code{\link{stairplot}}.
#' @param dat=basedata The \code{data.frame} holding study information
#' @param Arm='FullStudy' The study arm we want to plot
#' @return A \code{data.frame} object that can be input into \code{stairplot}
#' @section Details:
#' The input \code{data.frame} needs the following fields:
#' \itemize{
#' \item{'Author'}{Author}
#' \item{"pubdate"}{Publication date}
#' \item{"starts"}{The start time of the study (i.e. beginning of enrollment)}
#' \item{"ends"}{The end time of the study (i.e. max of end of enrollment, end of followup or a year before publication)}
#' \item{"yr_of_study"}{The analytic start time }
#' \item{"yr_of_study_end"}{The analytic end time}
#' \item{"Developed"}{Developed or developing country}
#' \item{"number"}{Size of the study (number of participants)}
#' \item{"Design"}{Study design (Observational or trial)}
#' }
stairdata <- function(dat=basedata, arm='FullStudy', group="Mixed"){
  require(dplyr)
  if(arm==''){
    dat2 <- dplyr::filter(dat, Group_class %in% group)
  } else {
    dat2 <- dplyr::filter(dat, Arm==arm & Group_class %in% group)
  }
  dat2 <- dat2 %>%
    select(study, Arm, Group_class, Author, pubdate, starts, yr_of_study, yr_of_study_end, ends, Developed, Design, number) %>%
    mutate(Design=as.factor(ifelse(Design=='Trial', 'Trial','Observational')),
           labels=paste(Author, pubdate, sep=',')) %>%
    arrange(starts)
  return(dat2)
}

#' Plotting "staircase" plots that show the temporal extent of each study
#' 
#' This function takes a summary data.frame with start and end dates of the study as well as start and
#' end dates of the analytic window contributed by the study, and plots it, sorted by start date
#' 
#' @author Abhijit Dasgupta <abhijit.dasgupta@nih.gov>
#' @param d A \code{data.frame} object created by \code{\link{stairdata}}
#' @param title='' The plot title
#' @param lims = NULL The x axis limits as a 2-vector, giving the year span for the plot
#' @return A ggplot object
stairplot <- function(d, title='',lims=NULL){
  require(ggplot2)
  ggplot(d, aes(x = seq_along(yr_of_study), y=yr_of_study, ymin=starts, ymax=ends))+
    geom_point(aes(size=sqrt(number), color=Design))+
    geom_linerange()+
    geom_linerange(aes(x=seq_along(yr_of_study), ymin=yr_of_study, ymax=yr_of_study_end), colour='green')+
    labs(x='' )+guides(size=FALSE)+
    geom_text(aes(x=seq_along(yr_of_study),y=starts-0.5, label=labels, size=4, hjust=1))+
    #coord_flip()+
    theme(legend.position='bottom', axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    scale_y_continuous('Year', breaks=seq(1950,2010,by=10), limits=lims)+
    coord_flip()+
    ggtitle(title)
}

# On closing --------------------------------------------------------------


unlink('data/xls',recursive=T)
unlink('data/fromPapers',recursive=T)