load('data/rda/kmdata2.rda')
load('data/rda/summarySurv.rda')
load('data/rda/followup.rda')
load('data/rda/WardTableFinal.rda')
load('data/rda/survival1.rda')
load('data/rda/overallipd.rda')
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


class4data <- dplyr::filter(basedata, Group_class=='IV') %>% select(study, Author, Arm, number, Decades, Developed, Citation) # There are 4 studies (19, 26, 141, 148)where the entire study is class IV, and has multple arms. We need 1 row per study
class4data <- ddply(class4data, ~study, function(x) if(nrow(x)>1) dplyr::filter(x, Arm=='FullStudy') else x)
# There are 2 kinds of studies -- those where full study is single group, and those with a single arm being class IV. Separate the cases
class4km1 <- intersect(names(kmdata), dplyr::filter(class4data, Arm=='FullStudy')$study)
# We'll get ipd from overall.ipd later
class4km2 <- intersect(names(kmdata), dplyr::filter(class4data, Arm !='FullStudy')$study)
bl <- ldply(kmdata[as.character(class4km2)], dplyr::filter, 
            Group %in% c('IV','class IV','Class4')) # only 8 studies had Class4 specific KM info
class4kmdata <- bl
class4kmdata <- lapply(split(class4kmdata, class4kmdata$.id), select, Group, Year, Prob)
class4surv <- intersect(names(survival_data), class4data$study)
class4surv <- setdiff(class4surv, c(class4km1,as.numeric(names(class4kmdata))))
class4datasurv <- dplyr::filter(class4data, study%in%class4surv)
blah <- vector('list', length(class4surv))
for(i in 1:length(class4surv)){
  blah[[i]] <- subset(survival_data[[as.character(class4datasurv$study[i])]],
                      Arm == class4datasurv$Arm[i])
}
names(blah) = as.character(class4datasurv$study)
class4survdata <- ldply(blah)
class4survdata <- lapply(split(class4survdata, class4survdata$.id), function(x) x[,-1])
# 7/14 had class 4 specific survival information


ipdstudies = Reduce(union, list(class4km1,as.numeric(names(class4kmdata)), as.numeric(names(class4survdata))))
followup_IV <- followup_all[intersect(names(followup_all),as.character(setdiff(class4data$study, ipdstudies)))]
blah <- vector('list',length(followup_IV))
for(i in 1:length(blah)){
  print(i)
  stdy <- names(followup_IV)[i]
  blah[[i]] <- subset(followup_IV[[i]], Arm==class4data[class4data$study==stdy,'Arm'])
}
names(blah) <- names(followup_IV)
class4follow <- ldply(blah)
class4follow <- dlply(class4follow, ".id",strip_splits)




# Creating IPD ------------------------------------------------------------

study.ipd <- vector('list',length(class4kmdata))
for(i in 1:length(class4kmdata)){
  print(i)
  stdy <- as.numeric(names(class4kmdata)[i])
  arm <- class4data$Arm[class4data$study==stdy]
  n <- get.n.risk(stdy, basedata, arm)
  study.ipd[[i]] <- get.IPD(KMclean(class4kmdata[[i]],n),n)
}
names(study.ipd) <- names(class4kmdata)

study.ipd <- c(study.ipd, overall.ipd[as.character(class4km1)]) # include overall ipd's for full study studies

fnlist = list(n.risk=get.n.risk, n.events=get.tot.events, maxfollowup=get.maxfollowup, lags=get.lag)

for(i in 1:length(class4survdata)) class4survdata[[i]] <- transform(class4survdata[[i]], study=as.numeric(names(class4survdata)[i]))

surv.ipd <- lapply(class4survdata, getIPD.summ)
surv.ipd <- surv.ipd[!sapply(surv.ipd,is.null)]
study.ipd <- c(study.ipd, surv.ipd) # 18 studies

save(class4data,class4km1,class4km2, class4kmdata, class4survdata, class4follow, study.ipd, file='data/rda/class4mcmc.rda',compress=T)
# unlink('data/fromPapers', recursive=T)
# unlink('data/xls', recursive=T)

# instudy <- Reduce(union, list(names(study.ipd), names(class4follow)))
# bl <- dplyr::filter(class4data, study %in% as.numeric(instudy))
# arrange(merge(select(basedata, study,Arm,Group_class, Author, Citation), bl[,c('study','Arm')], by=c('study','Arm')), study)
# 
# # study       Arm Group_class          Author                                   Citation
# # 1      1   1c (IV)          IV Gruppo Italiano                        AJKD 1992; 19:473-9
# # 2      2 FullStudy          IV         Abraham                        JAPI 1999; 47:862-5
# # 3      3     3e IV          IV           Adler           Rheumatology 2006;45:1144â€“1147
# # 4      4   4c (IV)          IV        Al Arfaj             Rheumatol Int 2009; 29:1057-67
# # 5      6 FullStudy          IV    Arce-salinas                      Lupus 1995; 4:197-203
# # 6      8   8c (IV)          IV           Bakir                       AJKD 1994; 24:159-71
# # 7      9   9b (IV)          IV         Baldwin                    Am J Med 1977; 62:12-30
# # 8     12   12d, IV          IV            Beji           La rev Med Interne 2005; 26:8-12
# # 9     13  13c (IV)          IV         Belmont                        Lupus 1995; 4:104-8
# # 10    19 FullStudy          IV            Cade                     Nephron 1973; 10:37-56
# # 11    23       23b          IV            Chan               Med J Malaysia 2000;55:14-20
# # 12    24       24a          IV         Chan Tm                     Nephron 1995; 71:321-7
# # 13    26 FullStudy          IV         Chan Tm                      JASN 2005; 16:1076-84
# # 14    29       29c          IV             Chu                  Chin Med J 1994; 53:27-36
# # 15    30 FullStudy          IV           Clark                       CMAJ 1981; 125:171-4
# # 16    31 FullStudy          IV           Clark    Plasma Ther Transf Tech 1984; 5:353-60 
# # 17    33 FullStudy          IV           Conte               Nouv Presse Med 1975; 4:91-5
# # 18    37       37a          IV          D'cruz             Clin Exp Rheum 1997; 15:275-82
# # 19    43       43d          IV         Derksen                       Lupus 1992; 1:97-103
# # 20    45       45c          IV    Djukanovitsh             Serbian 2002; 130(Supp3):26-31
# # 21    46  46c (IV)          IV         Donadio                       Lupus 1995; 4:109-15
# # 22    47 FullStudy          IV           Doria           Ann Med Interne 1994; 145:307-11
# # 23    52       52a          IV          Faedda               Clin Nephrol 1995; 44:367-75
# # 24    56  56c (IV)          IV           Gamba            Rev Invest Clin 2000; 52:397-05
# # 25    58 FullStudy          IV        Genovese            Arthritis Rheum 2002; 46:1014-8
# # 26    67 FullStudy          IV       Hariharan                 Clin Nephrol 1990; 34:61-9
# # 27    73       73c          IV           Huong                    Medicine 1999 78:148-66
# # 28    79 FullStudy          IV           Kanno                  Ren Failure 2007; 29:41-7
# # 29    84       84a          IV         Klinger            Pol Arch Med Wewn 1994; 92:70-7
# # 30    86       86c          IV          Leaker                        QJM 1987; 62:163-79
# # 31    87 FullStudy          IV          Leehey              AJKD 1982; 11(Suppl 1):188-96
# # 32    89       89b          IV           Magil                   Am J Med 1982; 72:620-30
# # 33    98 FullStudy          IV          Mittal            Am J Kidney Dis 2004; 44:1050-9
# # 34   100      100d          IV             Mok            Am J Kidney Dis 1999; 34:315-23
# # 35   101 FullStudy          IV             Mok                 Am J Med 2006; 119:355:e25
# # 36   106 FullStudy          IV           Mosca                    Kid Int 2002; 61:1502-9
# # 37   107      107b          IV          Najafi                   Kid Int 2001; 59:2156-63
# # 38   110      110d          IV         Nossent             Arthritis Rheum 1990; 33:970-7
# # 39   123      123c          IV            Shen                 Chin Med J 1997; 110:502-7
# # 40   129 FullStudy          IV            Tang                Rheumatol Int 2009; 30:45-9
# # 41   141 FullStudy          IV          Dooley                   Kid Int 1997; 51:1188-95
# # 42   142      142d          IV         Ferluga     Wein Klin Wochenschr 2000; 112:692-701
# # 43   144      144d          IV           Howie                        QJM 2003; 96:411-20
# # 44   147 FullStudy          IV             Tam                        QJM 1998; 91:573-80
# # 45   149      149c          IV           Appel                   Am J Med 1987; 83:877-85
# # 46   165 FullStudy          IV        Mitwalli Nephrol Dial Transplant 2012; 27: 627â€“32
# # 47   166      166b          IV         Ayodele         Int Urol Nephrol 2013; 45:1289-300
# # 48   169      169b          IV         Haddiya       Int J Nephrol Renovas 2013; 6:249-58
# 
# bl <- dplyr::filter(class4data, !(study %in% as.numeric(instudy)))
# arrange(merge(select(basedata, study,Arm, Group_class,Author, Citation), bl[,c('study','Arm')], by=c('study','Arm')), study)
# 
# # study       Arm Group_class  Author                           Citation
# # 1    93       93b          IV Martins        Clin Nephrol 2002; 57:114-9
# # 2   108      108b          IV Neumann Sem Arthritis Rheum 1995; 25:47-55
# # 3   148 FullStudy          IV   Eiser        Clin Nephrol 1993; 40:155-9
# # 4   158      158b          IV   Singh    Am J Med Sci 2011;342:467â€“473
# 
# # Study 108 doesn't have IPD since number of events are missing
# 
# class4data <- dplyr::filter(class4data, study %in% as.numeric(instudy))
# # Decades
# # Developed    < 1979 1980-1989 1990-1999 2000-2009
# # Developed      10        11         8         0
# # Developing      1         6         8         4
# 
# # Set up for JAGS ---------------------------------------------------------
# 
# 
# class4data1 <- merge(class4data, 
#                     basedata[,c('study','Arm','lags')], 
#                     by=c('study','Arm'))
# 
# data.jags <- as.data.frame(gen.jagsdata(study.ipd, class4data1))
# 
# class4follow1 <- ldply(class4follow)
# out <- merge(class4follow1[,c('.id','Arm','maxfollowup','Events')], basedata[,c('study','Arm','lags','number','Developed','Decades')], by.x=c('.id','Arm'), by.y=c('study','Arm'))
# out$lags <- ifelse(is.na(out$lags), 0, out$lags)
# out$isCensored2 <- as.integer(rep(1, nrow(out)))
# out$Y <- rep(NA, nrow(out))
# names(out)[c(1,6:8)] <- c('study','n','geog2','yr2')
# out$yr2 <- as.factor(out$yr2)
# out$geog2 <- as.factor(out$geog2)
# 
# data.class4 <- as.list(out[,-(1:2)])
# data.class4$N2 <- nrow(out)
# names(data.jags) <- c('td','tcens','trunc','isCensored1','geog1','yr1')
# data.jags$yr1 <- as.factor(data.jags$yr1)
# data.jags$geog1 <- as.factor(data.jags$geog1)
# 
# data.class4 <- c(data.class4, as.list(data.jags))
# data.class4$N1 <- nrow(data.jags)
# 
# for(i in 1:length(data.class4)){
#   if(is.factor(data.class4[[i]])) data.class4[[i]] <- as.numeric(data.class4[[i]])
# }
# data.class4$J <- length(unique(out$geog2))
# data.class4$K <- length(unique(out$yr2))
# 
# dump('data.class4', file='fullstudy.class4.dat')
# 
# # JAGS model --------------------------------------------
# source('fullstudy.class4.dat')
# library(random)
# require(rjags)
# library(doParallel)
# cl <- makeCluster(2)
# registerDoParallel(cl)
# library(foreach)
# jags.parsamples <- foreach(i=1:2, .packages=c('rjags','random')) %dopar%{
#   load.module('glm')
#   load.module('lecuyer')
#   inits <- with(data.class4, 
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
#                     data = data.class4,
#                     inits = inits,
#                     n.chains = 1, # Change to 4 after testing
#                     n.adapt = 1000)
#   #update(mod, n.iter=1000) # Burn-in
#   codaSamples <- coda.samples(mod, variable.names=parameters, n.iter=1000, thin=1)
#   
#   boxplot(as.data.frame(codaSamples[[1]])[,c(33,35,37,39)])
#   
#   return(codaSamples)
# }
# 
