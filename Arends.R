# Arends reconstruction
library(readr)
surv <- rep(104,71)
cens <- rep(0, 71)
d <- data.frame(surv=surv, cens=cens)

d[1,] <- c(87,1) # Only ESRD case
d[2:11,1] = c(3,5,13,16,27,24,38,117,90,104)

library(survival)
#plot(survfit(Surv(surv,cens)~1, data=d))
s = summary(survfit(Surv(surv,cens)~1, data=d), times=0:104)
out = data.frame(Time = s$time/52, Probability=s$surv)
untar('data/fromPapers.tar.gz', exdir='data')
write_csv(out, path='data/fromPapers/Arends.csv')
tar('data/fromPapers.tar.gz', files='data/fromPapers', compression='gzip')
unlink('data/fromPapers', recursive = TRUE)
