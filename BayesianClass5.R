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


class5data <- dplyr::filter(basedata, Group_class=='V') %>% select(study, Author, Arm, number, Developed, Decades, Citation) # There are 4 studies (19, 26, 141, 148)where the entire study is class V, and has multple arms. We need 1 row per study
class5data <- ddply(class5data, ~study, function(x) if(nrow(x)>1) dplyr::filter(x, Arm=='FullStudy') else x)
# There are 2 kinds of studies -- those where full study is single group, and those with a single arm being class V. Separate the cases
class5km1 <- intersect(names(kmdata), dplyr::filter(class5data, Arm=='FullStudy')$study)
class5kmdata <- ldply(kmdata[as.character(class5km1)], dplyr::filter, 
                      Group=='FullStudy' | Group == "Overall") 
class5km2 <- intersect(names(kmdata), dplyr::filter(class5data, Arm !='FullStudy')$study)
bl <- ldply(kmdata[as.character(class5km2)], dplyr::filter, 
            Group %in% c('V','class V','class5'))
class5kmdata <- rbind(class5kmdata, bl)
class5kmdata <- lapply(split(class5kmdata, class5kmdata$.id), select, Group, Year, Prob)
class5surv <- intersect(names(survival_data), class5data$study)
class5surv <- setdiff(class5surv, names(class5kmdata))
class5datasurv <- dplyr::filter(class5data, study%in%class5surv)
blah <- vector('list', length(class5surv))
for(i in 1:length(class5surv)){
  blah[[i]] <- subset(survival_data[[as.character(class5datasurv$study[i])]],
                      Arm == class5datasurv$Arm[i])
}
names(blah) = as.character(class5datasurv$study)
class5survdata <- ldply(blah)
class5survdata <- lapply(split(class5survdata, class5survdata$.id), function(x) x[,-1])

followup_V <- followup_all[intersect(names(followup_all),
                                     as.character(setdiff(class5data$study, 
                                                          names(c(class5kmdata,class5survdata)))))]
blah <- vector('list',length(followup_V))
for(i in 1:length(blah)){
  stdy <- names(followup_V)[i]
  blah[[i]] <- subset(followup_V[[i]], Arm==dplyr::filter(class5data, study==stdy)$Arm)
}
names(blah) <- names(followup_V)
class5follow <- ldply(blah)
class5follow <- dlply(class5follow, ".id",strip_splits)


# instudy <- c(names(class5kmdata),names(class5survdata), names(class5follow))
# bl <- dplyr::filter(class5data, study %in% instudy)
# arrange(merge(select(basedata, study,Arm,Group_class, Author, Citation), bl[,c('study','Arm')], by=c('study','Arm')), study)
# 
# # study       Arm Group_class               Author                                Citation
# # 1      1    1d (V)           V      Gruppo Italiano                     AJKD 1992; 19:473-9
# # 2      3      3f V           V                Adler        Rheumatology 2006;45:1144â€“1147
# # 3      4    4d (V)           V             Al Arfaj          Rheumatol Int 2009; 29:1057-67
# # 4      8    8d (V)           V                Bakir                    AJKD 1994; 24:159-71
# # 5      9    9c (V)           V              Baldwin                 Am J Med 1977; 62:12-30
# # 6     12   12e (V)           V                 Beji        La rev Med Interne 2005; 26:8-12
# # 7     13   13d (V)           V              Belmont                     Lupus 1995; 4:104-8
# # 8     23       23c           V                 Chan            Med J Malaysia 2000;55:14-20
# # 9     24       24b           V              Chan Tm                  Nephron 1995; 71:321-7
# # 10    25 FullStudy           V              Chan Tm                    Lupus 1999; 8:545-51
# # 11    27 FullStudy           V               Chen Q               J Nephrol 2003; 16:126-32
# # 12    28       28a           V           Chrysochou                     QJM 2008; 101:313-6
# # 13    43       43e           V              Derksen                    Lupus 1992; 1:97-103
# # 14    45   45d (V)           V         Djukanovitsh          Serbian 2002; 130(Supp3):26-31
# # 15    46   46d (V)           V              Donadio                    Lupus 1995; 4:109-15
# # 16    52       52b           V               Faedda            Clin Nephrol 1995; 44:367-75
# # 17    56   56d (V)           V                Gamba         Rev Invest Clin 2000; 52:397-05
# # 18    66 FullStudy           V             Hallegua                    Lupus 2000; 9:241-51
# # 19    72 FullStudy           V                   Hu            Chin Med J 2003; 116:1827-30
# # 20    73       73d           V                Huong                 Medicine 1999 78:148-66
# # 21    86       86d           V               Leaker                     QJM 1987; 62:163-79
# # 22    96 FullStudy           V             Mercadal       Neph Dial Transpl 2002; 17:1771-8
# # 23   100      100e           V                  Mok         Am J Kidney Dis 1999; 34:315-23
# # 24   102 FullStudy           V                  Mok                   Lupus 2009; 18:1091-5
# # 25   108      108c           V              Neumann      Sem Arthritis Rheum 1995; 25:47-55
# # 26   110      110e           V              Nossent          Arthritis Rheum 1990; 33:970-7
# # 27   113 FullStudy           V             Pasquali            Clin Nephrol 1993; 39:175-82
# # 28   114 FullStudy           V               Pasten           Rev Med Chile 2005; 133:23-32
# # 29   123      123d           V                 Shen              Chin Med J 1997; 110:502-7
# # 30   125 FullStudy           V Sqalli Houssaini, T. Presse Med. 2008 Apr;37(4 Pt 1):559-63.
# # 31   127 FullStudy           V                  Sun                    Lupus 2008; 17:56-61
# # 32   138      138f           V             Yokoyama                 Kid Int 2004; 66:2382-8
# # 33   142      142e           V              Ferluga  Wein Klin Wochenschr 2000; 112:692-701
# # 34   144      144e           V                Howie                     QJM 2003; 96:411-20
# # 35   149      149d           V                Appel                Am J Med 1987; 83:877-85
# # 36   159 FullStudy           V                  Yap           Nephrology 2012;17: 352â€“357
# 
# bl <- dplyr::filter(class5data, !(study %in% instudy))
# arrange(merge(select(basedata, study,Arm, Group_class,Author, Citation), bl[,c('study','Arm')], by=c('study','Arm')), study)
# 
# # study Arm Group_class  Author                    Citation
# # 1    93 93c           V Martins Clin Nephrol 2002; 57:114-9
# 
# Creating IPD ------------------------------------------------------------

study.ipd <- vector('list',length(class5kmdata))
for(i in 1:length(class5kmdata)){
  stdy <- as.numeric(names(class5kmdata)[i])
  arm <- class5data$Arm[class5data$study==stdy]
  n <- get.n.risk(stdy, basedata, arm)
  study.ipd[[i]] <- get.IPD(KMclean(class5kmdata[[i]],n),n)
}
names(study.ipd) <- names(class5kmdata)

fnlist = list(n.risk=get.n.risk, n.events=get.tot.events, maxfollowup=get.maxfollowup, lags=get.lag)

for(i in 1:length(class5survdata)) class5survdata[[i]] <- transform(class5survdata[[i]], study=as.numeric(names(class5survdata)[i]))

surv.ipd <- lapply(class5survdata, getIPD.summ)
surv.ipd <- surv.ipd[!sapply(surv.ipd,is.null)]
study.ipd <- c(study.ipd, surv.ipd)

class5data <- dplyr::filter(class5data, study %in% as.numeric(c(names(study.ipd),names(class5follow))))

save(class5data, class5kmdata, class5survdata, class5follow, study.ipd, file='data/rda/class5mcmc.rda',compress=T)
unlink('data/fromPapers', recursive=T)
unlink('data/xls', recursive=T)


# # Set up for JAGS ---------------------------------------------------------
# 
# class5data1 <- merge(class5data, 
#                     basedata[,c('study','Arm','Developed','Decades','lags')], 
#                     by=c('study','Arm'))
# data.jags <- as.data.frame(gen.jagsdata(study.ipd, class5data1))
# 
# class5follow1 <- ldply(class5follow)
# out <- merge(class5follow1[,c('.id','Arm','maxfollowup','Events')], basedata[,c('study','Arm','lags','number','Developed','Decades')], by.x=c('.id','Arm'), by.y=c('study','Arm'))
# out$lags <- ifelse(is.na(out$lags), 0, out$lags)
# out$isCensored2 <- as.integer(rep(1, nrow(out)))
# out$Y <- rep(NA, nrow(out))
# names(out)[c(1,6:8)] <- c('study','n','geog2','yr2')
# out$yr2 <- as.factor(out$yr2)
# 
# out$geog2 <- as.factor(out$geog2)
# 
# data.class5 <- as.list(out[,-(1:2)])
# data.class5$N2 <- nrow(out)
# data.class5$J <- length(levels(out$geog2))
# data.class5$K <- length(levels(out$yr2))
# names(data.jags) <- c('td','tcens','trunc','isCensored1','geog1','yr1')
# data.jags$yr1 <- as.factor(data.jags$yr1)
# data.jags$geog1 <- as.factor(data.jags$geog1)
# 
# data.class5 <- c(data.class5, as.list(data.jags))
# data.class5$N1 <- nrow(data.jags)
# 
# for(i in 1:length(data.class5)){
#   if(is.factor(data.class5[[i]])) data.class5[[i]] <- as.numeric(data.class5[[i]])
# }
# 
# dump('data.class5', file='fullstudy.class5.dat')
# 
# # JAGS model --------------------------------------------
# source('fullstudy.class5.dat')
# library(random)
# require(rjags)
# library(foreach)
# jags.parsamples <- foreach(i=1:2, .packages=c('rjags','random')) %dopar%{
#   load.module('glm')
#   load.module('lecuyer')
#   inits <- with(data.class5, 
#                 list(beta = matrix(0, J, K),
#                      nu = matrix(1.5, J, K),
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
#                     data = data.class5,
#                     inits = inits,
#                     n.chains = 1, # Change to 4 after testing
#                     n.adapt = 1000)
#   #update(mod, n.iter=1000) # Burn-in
#   codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
#   
#   return(codaSamples)
# }
