## Modifications of Kaplan Meier data as needed
untar('data/fromPapers.tar.gz')#, exdir = 'data')
setwd('data/fromPapers')

# Abraham -----------------------------------------------------------------
dat <- read.csv('Abraham.csv', header=F)
names(dat) <- c('Year','Survival')
dat <- transform(dat, Year= pmax(Year, 0), Survival = pmin(Survival,1))
dat <- dat[order(dat$Year),]
# These are for hypertensives, who are 65.5% of study population. None of the non-hypertensives had event, so we get weighted results as a convex combination
dat$Survival2 <- 0.655*dat$Survival + (1-0.655)
write.csv(dat, file='Abraham_modified.csv', row.names=F)


# Bono --------------------------------------------------------------------

## Convert from incidence to survival

bono <- read.csv('data/fromPapers/Bono.csv')
bono2 <- transform(bono, Prob = 1-Prob)
write.csv(bono2, file='data/fromPapers/Bono2.csv', row.names=F)

# Carette -----------------------------------------------------------------

# Uncertainty in digitization is creating problems in terms of accurate at.risk numbers. Moving at.risk time from
# 2.5 to 2.45 to accomodate


# Faedda ------------------------------------------------------------------

## Primary data in FAEDDAindiv.txt
filedir = 'data/fromPapers'
dat <- read.table(file.path(filedir, 'FAEDDAindiv.txt'),header=T)
dat <- transform(dat, FUyr = FUmo/12, cens = ifelse(Outcome=='remission',0,1))
require(survival)
sf <- survfit(Surv(FUyr,cens)~1, data=dat)
f <- file.path(filedir, 'Faedda.csv')
out <- data.frame(Year = sf$time, Prob = sf$surv)
write.csv(out,f, row.names=F)
f <- file.path(filedir, 'FaeddaAtRisk.csv')
out=data.frame(Year=sf$time, "Number at risk"=sf$n.risk)
write.table(out,file=f, sep=',',row.names=F, col.names = c('Year','Number at risk'))
system(paste('python addSheet.py',f, 'data/Summary.xls','Faedda','data/Summary.xls'))


# Siso --------------------------------------------------------------------

siso <- read.csv('data/fromPapers/Siso.csv')
siso <- siso[,-1]
siso$Prob  <- 1-siso$Cumulative.incidence
siso = siso[,c("Group","Years","Prob")]
names(siso) <- c('Group','Year','Prob')
write.csv(siso, file='data/fromPapers/Siso.csv',row.names=F)

############################################################################
# File modifications during data normalization in sandbox
############################################################################
datadir <- 'data/sandbox'
# Mitwalli ----------------------------------------------------------------
dat <- read.csv(file.path(datadir, 'Mitwalli.csv'), header=T)
dat <- dat[,-2]
names(dat) <- c("Group",'Year','Prob')
write.table(dat, file=file.path(datadir, 'Mitwalli.csv'), row.names=F, sep=',')
file.copy(file.path(datadir, 'Mitwalli.csv'), 'data/fromPapers/Mitwalli.csv', overwrite=T)

dat <- read.csv(file.path(datadir, 'Mitwalli0.csv'), header=T)
dat <- dat[,-2]
names(dat) <- c('Group','Year','Prob')
write.table(dat, file=file.path(datadir, 'Mitwalli0.csv'), row.names=F, sep=',')
file.copy(file.path(datadir, 'Mitwalli0.csv'), 'data/fromPapers/Mitwalli0.csv', overwrite=T)

# Najafi ------------------------------------------------------------------

dat <- read.csv(file.path(datadir, 'Najafi.csv'))
dat <- dat[,-1]
names(dat) <- c('Group','Year','Prob')
write.table(dat, file=file.path(datadir, 'Najafi.csv'), row.names=F, sep=',')
file.copy(file.path(datadir, 'Najafi.csv'), 'data/fromPapers/Najafi.csv', overwrite=T)

