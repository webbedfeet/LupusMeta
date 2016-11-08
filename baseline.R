
# Ensure you start new R session before running.
# 
# Patterns in baseline characteristics over time ---------------------------
reload()
# load('data/rda/bookkeeping.rda')
load('data/rda/WardTableFinal.rda')

# Compute study year as middle of enrollment ------------------------------

timedat <- basedata[,c('start','endfu','endenroll')]
yr_of_study <- round(with(timedat, (start + pmin(endfu, endenroll, na.rm=T))/2))
ind <- which(is.na(yr_of_study))
yr_of_study[ind] <- ifelse(is.na(timedat$start[ind]),NA, round((basedata$pubdate[ind]-1+timedat$start[ind])/2))
ind <- which(is.na(yr_of_study))
yr_of_study[ind] <- round(pmin(basedata$endfu[ind],basedata$pubdate[ind]-1,na.rm=T) - pmax(basedata$esrdfu..m.[ind],basedata$maxfollowup[ind],na.rm=T)/12/2)
ind <- is.na(yr_of_study) # All left is study 118
yr_of_study[ind] <- rep(yr_of_study[basedata$study==118 & !ind],sum(ind))
basedata$yr_of_study <- yr_of_study

# We want to see if the serum creatinine level and the lag to treatment has changed over time,
# which could partially explain the flatness of the ESRD rate
end_date <- with(basedata, pmax(endfu, endenroll, pubdate-1, na.rm=T))
start_date <- basedata$yr_of_study

x <- tbl_df(basedata)
x1 <- x %>% 
  mutate(start_date = yr_of_study, 
         end_date = pmax(endfu, endenroll, pubdate-1, na.rm=T)) %>%
  dplyr::select(study, Arm, start_date, end_date,Group_class) %>%
  dplyr::filter(Arm=='FullStudy')


coverage <- vector('list',nrow(x1))
for(i in 1:nrow(x1)){
  coverage[[i]] <- seq(x1$start_date[i], x1$end_date[i])
}
names(coverage) <- as.character(x1$study)

Windows = matrix(0, ncol=5, nrow=1)
for(i in 1964:2011){
  Windows <- rbind(Windows, seq(i, i+4))
}
Windows <- Windows[-1,]

books <- matrix(0, ncol=length(coverage), nrow=nrow(Windows))
for(j in 1:length(coverage)){
  for(i in 1:nrow(Windows)){
    books[i,j] <- length(intersect(coverage[[j]], Windows[i,]))>0
  }
}
books <- as.data.frame(books)
names(books) <- names(coverage)
members <- vector('list',nrow(books))
for(i in 1:nrow(books)){
  members[[i]] <- names(books)[books[i,]==1]
}

geog <- lapply(members, function(x) dplyr::filter(basedata, study %in% as.numeric(x), Arm=='FullStudy')%>%dplyr::select(Developed))

lags.by.window <- lapply(members,get.lag, basedata )
nrisk.by.window <- lapply(members, get.n.risk, basedata, arm='FullStudy')
sCr.by.window <- lapply(members, get.sCr, basedata, arm='FullStudy')

tmp <- mapply(cbind, nrisk.by.window, lags.by.window, SIMPLIFY=FALSE)
by.window <- mapply(cbind,tmp, sCr.by.window, SIMPLIFY=FALSE)


save(list=c('members','geog','coverage','Windows', 'lags.by.window','nrisk.by.window','by.window'), file='data/rda/bookkeeping.rda')

# out <- data.frame(t(sapply(by.window, function(d) sapply(d[,3:4], weighted.mean, w=d$number, na.rm=T))))
# # out2 <- data.frame(t(sapply(by.window, function(d) sapply(d[,3:4], mean, na.rm=T))))
# out$yr <- Windows[,3]
# # out2$yr <- Windows[,3]
# 
# library(ggplot2)
# library(reshape2)
# p1 <- ggplot(out, aes(x=yr, y=sCr))+
#   geom_line(size=1)+
#   ylim(c(0,2))+
#   labs(x='Year',y='Serum Creatinine')
# p2 <- ggplot(out, aes(x=yr, y=lags))+
#   geom_line(size=1)+
#   scale_y_continuous(name='Average time from disease onset to study (years)',
#                      breaks=0:10,
#                      limits=c(0,2))+
#   labs(x='Year')


# pdf('graphs/baseline.pdf')
# print(p1)
# print(p2)
# dev.off()


# ggplot(out2, aes(x=yr, y=sCr))+
#   geom_line()+
#   ylim(range(do.call(rbind,by.window)$sCr, na.rm=T))
# ggplot(out2, aes(x=yr, y=lags))+
#   geom_line()+
#   ylim(range(do.call(rbind, by.window)$lags, na.rm=T))



