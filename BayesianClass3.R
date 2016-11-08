load('data/rda/kmdata2.rda')
load('data/rda/summarySurv.rda')
load('data/rda/followup.rda')
load('data/rda/WardTableFinal.rda')
load('data/rda/survival1.rda')
if('fn' %in% search()) detach('fn')
fn <- new.env()
source('functions.R',local=fn)
source('truncFunctions.R',local=fn)
source('KMcleaning.R', local=fn)
attach(fn)

library(dplyr)
library(plyr)

followup_all <- basedata[,c('study','Arm', 'Author','Group_class','Group_drug','Group_race','maxfollowup','Events')]
followup_all <- dplyr::filter(followup_all, !is.na(maxfollowup))
followup_all <- dlply(followup_all, ~study, strip_splits)


class3data <- dplyr::filter(basedata, Group_class=='III') %>% select(study, Author, Arm, number, Developed, Decades, Citation) # All are unique studies
class3km <- intersect(names(kmdata), class3data$study)
class3kmdata <- ldply(kmdata[as.character(class3km)], dplyr::filter, Group=='III')
class3kmdata <- lapply(split(class3kmdata, class3kmdata$.id), select, Group, Year, Prob)
class3surv <- intersect(names(survival_data), class3data$study)
class3surv <- setdiff(class3surv, names(class3kmdata))
class3survdata <- ldply(survival_data[as.character(class3surv)], dplyr::filter, Group_class=='III')
class3survdata <- lapply(split(class3survdata, class3survdata$.id), function(x) x[,-1])

class3follow <- ldply(followup_all[intersect(names(followup_all),as.character(setdiff(class3data$study, 
	names(c(class3kmdata,class3survdata)))))], dplyr::filter, Group_class=='III')
class3follow <- dlply(class3follow, ".id",strip_splits)


# instudy <- Reduce(union, list(names(class3kmdata),names(class3survdata),
#                               names(class3follow)))
# 
# # These are the studies included in this analysis:
# # study       Arm          Author                               Citation
# # 1      1  1b (III) Gruppo Italiano                    AJKD 1992; 19:473-9
# # 2      3    3d III           Adler         Rheumatology 2006;45:1144–1147
# # 3      4  4b (III)        Al Arfaj         Rheumatol Int 2009; 29:1057-67
# # 4      8  8b (III)           Bakir                   AJKD 1994; 24:159-71
# # 5      9  9a (III)         Baldwin                Am J Med 1977; 62:12-30
# # 6     12 12c (III)            Beji       La rev Med Interne 2005; 26:8-12
# # 7     13 13b (III)         Belmont                    Lupus 1995; 4:104-8
# # 8     23       23a            Chan           Med J Malaysia 2000;55:14-20
# # 9     29       29b             Chu              Chin Med J 1994; 53:27-36
# # 10    43       43c         Derksen                   Lupus 1992; 1:97-103
# # 11    45       45b    Djukanovitsh         Serbian 2002; 130(Supp3):26-31
# # 12    46 46b (III)         Donadio                   Lupus 1995; 4:109-15
# # 13    56 56b (III)           Gamba        Rev Invest Clin 2000; 52:397-05
# # 14    73       73b           Huong                Medicine 1999 78:148-66
# # 15    86       86b          Leaker                    QJM 1987; 62:163-79
# # 16    89       89a           Magil               Am J Med 1982; 72:620-30
# # 17   100      100c             Mok        Am J Kidney Dis 1999; 34:315-23
# # 18   107      107a          Najafi               Kid Int 2001; 59:2156-63
# # 19   108      108a         Neumann     Sem Arthritis Rheum 1995; 25:47-55
# # 20   110      110c         Nossent         Arthritis Rheum 1990; 33:970-7
# # 21   123      123b            Shen             Chin Med J 1997; 110:502-7
# # 22   138      138c        Yokoyama                Kid Int 2004; 66:2382-8
# # 23   142      142c         Ferluga Wein Klin Wochenschr 2000; 112:692-701
# # 24   144      144c           Howie                    QJM 2003; 96:411-20
# # 25   149      149b           Appel               Am J Med 1987; 83:877-85
# # 26   166      166a         Ayodele     Int Urol Nephrol 2013; 45:1289-300
# # 27   169      169a         Haddiya   Int J Nephrol Renovas 2013; 6:249-58
# 
# 
# # These ClassIII studies don't have usable survival data
# # study  Arm  Author                      Citation
# # 1    93  93a Martins   Clin Nephrol 2002; 57:114-9
# # 2   158 158a   Singh Am J Med Sci 2011;342:467–473
# 
# Creating IPD ------------------------------------------------------------

study.ipd <- vector('list',length(class3kmdata))
for(i in 1:length(class3kmdata)){
  print(i)
  
  stdy <- as.numeric(names(class3kmdata)[i])
  arm <- class3data$Arm[class3data$study==stdy]
  n <- get.n.risk(stdy, basedata, arm)
  study.ipd[[i]] <- get.IPD(KMclean(class3kmdata[[i]],n),n)
}
names(study.ipd) <- names(class3kmdata)

fnlist = list(n.risk=get.n.risk, n.events=get.tot.events, maxfollowup=get.maxfollowup, lags=get.lag)

bl <- class3survdata#[!sapply(class3survdata, function(x) all(x$Prob==1))]

for(i in 1:length(bl)) bl[[i]] <- transform(bl[[i]], study=as.numeric(names(bl)[i]))

surv.ipd <- lapply(bl, getIPD.summ)
surv.ipd <- surv.ipd[!sapply(surv.ipd,is.null)]
# One study has missing number of events, so was omitted
study.ipd <- c(study.ipd, surv.ipd)

class3data <- dplyr::filter(class3data, study %in% as.numeric(c(names(study.ipd),names(class3follow))))
save(class3data, study.ipd, class3kmdata, class3survdata, class3follow, file='data/rda/class3mcmc.rda',compress=T)
# unlink('data/fromPapers',recursive=T)
# unlink('data/xls', recursive=T)

# # Set up for JAGS ---------------------------------------------------------
# 
# class3data1 <- merge(filter(class3data, study %in% as.numeric(names(study.ipd))), 
#                     basedata[,c('study','Arm','lags')], 
#                     by=c('study','Arm'))
# data.jags <- as.data.frame(gen.jagsdata(study.ipd, class3data1))
# 
# class3follow1 <- ldply(class3follow)
# out <- merge(class3follow1[,c('.id','Arm','maxfollowup','Events')], basedata[,c('study','Arm','lags','number','Developed','Decades')], by.x=c('.id','Arm'), by.y=c('study','Arm'))
# out$lags <- ifelse(is.na(out$lags), 0, out$lags)
# out$isCensored2 <- as.integer(rep(1, nrow(out)))
# out$Y <- rep(NA, nrow(out))
# names(out)[c(1,6:8)] <- c('study','n','geog2','yr2')
# out$yr2 <- as.factor(out$yr2)
# # levels(out$yr2)[1:2] <- rep('< 1979',2)
# out$geog2 <- as.factor(out$geog2)
# 
# data.class3 <- as.list(out[,-(1:2)])
# data.class3$N2 <- nrow(out)
# data.class3$J <- length(levels(out$geog2))
# data.class3$K <- length(levels(out$yr2))
# names(data.jags) <- c('td','tcens','trunc','isCensored1','geog1','yr1')
# data.jags$yr1 <- as.factor(data.jags$yr1)
# data.jags$geog1 <- as.factor(data.jags$geog1)
# 
# data.class3 <- c(data.class3, as.list(data.jags))
# data.class3$N1 <- nrow(data.jags)
# 
# for(i in 1:length(data.class3)){
#   if(is.factor(data.class3[[i]])) data.class3[[i]] <- as.numeric(data.class3[[i]])
# }
# 
# dump('data.class3', file='fullstudy.class3.dat')
# 
# # JAGS model --------------------------------------------
# source('fullstudy.class3.dat')
# library(random)
# require(rjags)
# 
# library(foreach)
# jags.parsamples <- foreach(i=1:2, .packages=c('rjags','random')) %dopar%{
#   load.module('glm')
#   load.module('lecuyer')
#   inits <- with(data.class3, 
#                 list(beta = matrix(0, J, K),
#                      nu = matrix(1, J, K),
#                      .RNG.name = "lecuyer::RngStream",
#                      .RNG.seed = randomNumbers(n=1, min=1, max=1e+06, col=1),
#                      td = ifelse(isCensored1==1, 100, NA), # init for censored td
#                      Y = ifelse(isCensored2==1, n, NA)# init for censored Y
#                 )
#   )
#   
#   parameters <- c('lambda','nu','pr5','pr10','pr15')
#   
#   mod <- jags.model("fullmodel.bug",
#                     data = data.class3,
#                     inits = inits,
#                     n.chains = 1, # Change to 4 after testing
#                     n.adapt = 1000)
#   #update(mod, n.iter=1000) # Burn-in
#   codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
#   
#   return(codaSamples)
# }
# 
# 
