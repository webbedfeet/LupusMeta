# Output for paper

reload()
load('data/rda/bookkeeping.rda')

# Numbers by strata -------------------------------------------------------
load('data/rda/WardTableFinal.rda')
load('data/rda/mixedmcmc.rda')
study.ipd <- c(keep.overall, keep.summ)
used <- c(followup.fullstudy$study, as.numeric(names(study.ipd)))
basedata %>% dplyr::filter(Arm=='FullStudy',Group_class=='Mixed', study %in% used) %>% 
  select(study, Developed) %>% count(Developed)


# Verification of numbers in text -----------------------------------------
fullstudy <- basedata %>% dplyr::filter(Arm=='FullStudy')

fullstudy %>% select(class3:class5) %>% apply(.,1,sum, na.rm=T) -> fullstudy$propbiopsy

# Aggregating summary data by strata --------------------------------

## Functions
#' Collapsing the summary MCMC data into a single data.frame
#'
#' This function inputs a hierarchical list of summary data (by strata) and
#' creates a single \code{\link{data.frame}} with columns specifying 
#' each stratum.
#'
#' @param out The output from mcmcout (in functions.R), the results from parsing analyses from a 
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


#' Plotting the results of continuous analysis, with panels by Developed status
#'
#' This function takes the results of \code{\link{collapseResults}} and plots them, with colors
#' representing incidence rates after 5, 10 and 15 years, dotted lines representing a 95% posterior
#' interval, and panels for Developed and Developing countries
#'
#' @param bl The results from collapseResults
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
                     legend.background=element_rect(fill="transparent"))+xlim(1970,2015)
  return(p)
}

# Ingesting the data ------------------------------------------------------


# Work-specific
basedir = file.path('data','mcmc')
# Home-specific
#basedir <- file.path('~','Dropbox','Work','Ward','Studies','LupusMeta','data','mcmc')

bl.full <- collapseResults(mcmcout('FullMixed', basedir))
bl3 <- collapseResults(mcmcout('class3', basedir))
bl4 <- collapseResults(mcmcout('class4', basedir))
bl5 <- collapseResults(mcmcout('class5', basedir))

blobs <- collapseResults(mcmcout('Observational', basedir))
bltrial <- collapseResults(mcmcout('Trial',basedir))

blhq <- collapseResults(mcmcout('HQ', basedir))


# KM participation --------------------------------------------------------

load('data/rda/bookkeeping.rda')
enoughKM <- function (study.ipd, class4data, members) {
  ipd4 <- as.numeric(names(study.ipd))
  ipd4_bydev <- class4data %>% dlply(~Developed, function(m) intersect(m$study, ipd4))
  ipd4_deved <- lapply(members, intersect, ipd4_bydev$Developed)
  ipd4_deving <- lapply(members, intersect, ipd4_bydev$Developing)
  ipd4_n <- data.frame(yr = 1964:2011, Developed = sapply(ipd4_deved, length), Developing = sapply(ipd4_deving,length))
  return(ipd4_n)
}


keep_enoughKM <- function (bl4, ipd4_n) {
  bl4[bl4$Dev == 'Developed' & bl4$yr %in% ipd4_n$yr[ipd4_n$Developed<1],3:5] <- NA
  bl4[bl4$Dev == 'Developing' & bl4$yr %in% ipd4_n$yr[ipd4_n$Developing<1],3:5] <- NA
  return(bl4)
}

load('data/rda/class3mcmc.rda')
bl3 <- collapseResults(mcmcout('class3', basedir))
e3 <- enoughKM(study.ipd, class3data, members)
bl3 <- keep_enoughKM(bl3, e3)

load('data/rda/class4mcmc.rda')
bl4 <- collapseResults(mcmcout('class4', basedir))
e4 <- enoughKM(study.ipd, class4data, members)
bl4 <- keep_enoughKM(bl4,e4)

load('data/rda/class5mcmc.rda')
bl5 <- collapseResults(mcmcout('class5', basedir))
e5 <- enoughKM(study.ipd, class5data, members)
bl5 <- keep_enoughKM(bl5, e5)

# FullMixed (Figure 2) ---------------------------------------------------------------

pdf('graphsForPaper2/Figure2.pdf', width=7, height=6.5)
pltResults(bl.full)+ylim(0,0.6)
dev.off()



# Drugs (Figure 3) --------------------------------------------------------

## Figure for sCr and lags are in baseline.R

load('data/rda/WardTableFinal.rda')
drugs <- c('pred','gconly','ctxnih','ctxeuro','ctxpo','aza','mmf','csa','rtx','othermed')
drugdata <- basedata[,c('study','Arm','Author','Citation','yr_of_study',drugs)]
drugdata <- dplyr::filter(drugdata, Arm=='FullStudy')
drugdata$ctx <- apply(drugdata[,c('ctxnih','ctxeuro','ctxpo')],1, sum, na.rm=T) # Summarize to CTX +/-
drugdata$otherdrugs <- 100-drugdata$gconly # all other drugs
drugdata <- join(drugdata, basedata[,c('study','Arm','number')]) # defaults to left_join

load('data/rda/bookkeeping.rda')
drugs.by.window <- lapply(members, function(x) subset(drugdata, study %in% as.numeric(x)))
out <- data.frame(t(sapply(drugs.by.window, function(d) sapply(d[,-c(1:5,18)], weighted.mean, w=d$number, na.rm=T))))
out$yr <- Windows[,3]
out <- melt(out[,c('yr','gconly','ctx','aza','mmf','csa')], id='yr')

pltdrugs <- ggplot(out, aes(x=yr, y=value, group=variable, color=variable))+geom_line(size=1.1)+
  labs(x='Year')+
  scale_color_hue(name='',
                  l=40,
                       breaks=c('gconly','ctx','aza','mmf','csa'),
                       #values=c('blue','green','red','purple','orange'),
                       labels=c('Glucocorticoids only',
                                'Cyclophosphamide',
                                'Azathioprine',
                                'Mycophenolate mofetil',
                                'Cyclosporine'))+
  scale_y_continuous('Use percentage', breaks=c(10,20,30,40,50))+
  theme(legend.position='right')+theme_bw()+theme(legend.key=element_blank())
pltdrugs <- pltdrugs+
  theme(legend.position=c(0.7,0.9), legend.background=element_rect(fill='transparent'))+
  guides(color=guide_legend(ncol=2))

pdf('graphsForPaper2/Figure3.pdf',width=8,height=6)
print(pltdrugs)
dev.off()


# # Biopsied studies (Figure 4) ---------------------------------------------
# 
# ## Ingesting the weights
# untar('data/xls.tar.gz',exdir='data')
# weights <- read.xls('data/xls/Histological class distribution.xlsx',1)
# weights <- weights[complete.cases(weights),]
# weights35 <- weights[3:5,]
# weights35$Percent <- as.numeric(weights35$Percent)
# wt <- scale(weights35$Percent, center=F, scale=sum(weights35$Percent))
# unlink('data/xls',recursive=TRUE)
# 
# ## Finding which results files are available
# x <- paste('Rfile',1:45,'.rda',sep='') # All possible filenames
# files <- data.frame(cbind(x,x,x)); names(files) = c('class3','class4','class5')
# 
# for(i in 1:45){
#   if(!file.exists(file.path('data','mcmc','class3',files$class3[i]))) files$class3[i]=NA
#   if(!file.exists(file.path('data','mcmc','class4',files$class4[i]))) files$class4[i]=NA
#   if(!file.exists(file.path('data','mcmc','class5',files$class5[i]))) files$class5[i]=NA
# }
# 
# ## Ingest
# #' Ingest data from all 3 classes by time window
# #'
# #' @param i The index of the time window
# #' @param files A character matrix of results .rda files available for each window x class combination
# #' @return A list where each element is the full MCMC data from each class and the ith window, converted to a data.frame
# ingest <- function(i, files){
#   require(coda)
#   bl <- vector('list',3)
#   for(j in 1:3){
#     if(!is.na(files[i,j])){
#       load(file.path('data','mcmc',paste('class',j+2,sep=''),files[i,j]))
#       bl[[j]] <- data.frame(as.matrix(codaSamples))
#     }
#   }
#   return(bl)
# }
# ## Normalize
# #' Normalize the data so that each data.frame has the same number of columns
# #'
# #' This function changes the names of the columns to be consistent. If there is data
# #' from only one stratum, it adds null data representing the other stratum, and standardizes the 
# #' column names
# #' 
# #' @param x A list of data.frame objects, each from one of class 3, 4, and 5 analyses
# #' @return A normalized version of the input
# normalize <- function(x){
#   if(is.null(x)) return(x)
#   if(length(grep('2',names(x)))==0){
#     names(x) <- paste(names(x),'[1]',sep='')
#     y <- data.frame(matrix(NA, nrow=nrow(x),ncol=ncol(x)))
#     names(y) <- gsub('\\[1\\]','\\[2\\]',names(x))
#     x <- cbind(x,y)
#   } else {
#     names(x) = gsub('\\.([1-2])\\.','\\[\\1\\]',names(x))
#   }
#   x <- cbind(id=row.names(x),x) # Add row names as a column for merging
#   return(x)
# }
# 
# #' <brief desc>
# #'
# #' <full description>
# #' @param x <what param does>
# #' @param y <what param does>
# #' @export
# #' @keywords
# #' @seealso
# #' @return
# #' @alias
# #' @examples \dontrun{
# #'
# #'}
# merge.all <- function(x,y){
#   merge(x,y, all=T, by=c('id','variable'))
# }
# #' Create a single data.frame from the class-based list of data.frames
# #'
# #' This function takes a list of normalized data, melts it so that
# #' data is in the "long" form, and then joins them so that data from each class
# #' forms a single column in a new data.frame
# #' 
# #' @param lst A list of normalized data.frames
# #' @result A single data.frame merging data from each class in 'long' format
# combineClasses <- function(lst){
#   require(reshape2)
#   ind <- sapply(lst, is.null)
#   if(all(ind)) return(NULL)
#   lst <- lst[!ind]
#   bl <- lapply(lst, melt, id.vars='id')
#   bl2 <- Reduce(merge.all, bl)
#   cols = (3:5)[!ind]
#   names(bl2)[-(1:2)] <- paste('class',cols,sep='')
#   return(bl2)
# }
# 
# # Create the mixture distribution
# #' Created a weighted mixture of distributions
# #'
# #' Given 3 distributions and the relative weights of the distributions,
# #' create a mixture distribution, accounting for missing data 
# #'
# #' @param x A data.frame, results of \code{combineClasses}
# #' @param weight The weights specifying the mixture proportions
# #' @result A vector giving the mixture distribution
# wt.mean <- function(x, weight=wt){
#   x1 = scale(x, center=F, scale=1/wt)
#   ind = apply(x,2, function(y) ifelse(is.na(y),0,1))
#   ind = scale(ind, center=F, scale=1/wt)
#   s = rowSums(x1, na.rm=T)
#   denom = rowSums(ind, na.rm=T)
#   return(s/denom)
# }
# 
# #' Creating an mcmc object that represents the biopsied mixture distribution
# #' 
# #' This function takes the merged data and creates an mcmc object that gives the mixture
# #' posterior distribution for each parameter, representing the totality of the biopsied studies
# #' 
# #' @param x The results from combineClasses
# #' @param weights The weights determining the mixture distribution, in order of class 3,4,5
# #' @return An mcmc object for the posterior mixture distribution.
# toBiopsied <- function(x, weights=wt){
#   require(reshape2)
#   require(coda)
#   if(is.null(x)) return(NULL)
#   misscol = setdiff(paste('class',3:5,sep=''), names(x))
#   for(u in misscol) x[,u] <- NA
#   x$biopsied <- wt.mean(x[,paste('class',3:5,sep='')], weight=weights)
#   out <- dcast(x[,c('id','variable','biopsied')],id~variable, value.var='biopsied')
#   out <- out[,-1] # Remove id variable
#   out <- out[,apply(out,2, function(u) !all(is.na(u)))] # Remove empty columns
#   return(as.mcmc(out))
# }
# 
# ## Computing the biopsied distributions
# 
# ### Initialization, creating data in same format as mcmcout
# library(coda)
# outcts <- vector('list',3)
# for(i in 1:3) outcts[[i]] <- vector('list',2)
# for(i in 1:3){
#   for(j in 1:2){
#     outcts[[i]][[j]] <- matrix(NA, ncol=3, nrow=45)
#   }
# }
# names(outcts) <- c('pr5','pr10','pr15')
# 
# ### Creating the data
# for(i in 1:45){
#   print(i)
#   bl <- ingest(i, files)
#   bl <- lapply(bl, normalize)
#   bl2 <- combineClasses(bl)
#   codaSamples <- toBiopsied(bl2, weights=wt)
#   if(is.null(codaSamples)) next
#   s <- summary(codaSamples)
#   quants <- s$quantiles
#   if(length(grep('\\[',row.names(quants)))==0){
#     for(u in paste('pr',c(5,10,15),sep='')){
#       outcts[[u]][[1]][i,] <- quants[u,c(1,3,5)]}
#   } else {
#     x <- row.names(quants)
#     x <- grep('pr',x,value=T)
#     nms = do.call(rbind,strsplit(gsub('\\]','',x),'\\['))
#     for(k in 1:nrow(nms)){
#       if(quants[x[k],3]==0)next
#       outcts[[nms[k,1]]][[as.numeric(nms[k,2])]][i,] <- quants[x[k],c(1,3,5)]
#     }
#   }
# }
# 
# bl.biopsied <- collapseResults(outcts)
# pdf('graphsForPaper/Figure4.pdf',width=7, height=6.5)
# pltResults(bl.biopsied)
# dev.off()

# By class ----------------------------------------------------------------

pltResults(bl3)+ggtitle('Class III')
pltResults(bl4)+ggtitle('Class IV')
pltResults(bl5)+ggtitle('Class V')

# Class x Dev panel (Figure 4) ---------------------------------------------
bl4[bl4$Dev=='Developing' & bl4$yr==1983, 3:5] <- NA
bl5[bl5$Dev=='Developing' & bl5$yr==2006, 3:5] <- NA

blah <- list('Class III'=bl3, "Class IV" = bl4, "Class V" = bl5)
bl.all <- ldply(blah, .id='Class')
levels(bl.all$Dev) <- paste(levels(bl.all$Dev),'countries')
levels(bl.all$Year) <- gsub('s','',levels(bl.all$Year))

pltpanel <- ggplot(bl.all, aes(x=yr, y=1-Med, group=Year, color=Year))+geom_line(size=1)+
  geom_line(aes(x=yr, y=1-LB),linetype=2)+
  geom_line(aes(x=yr, y=1-UB), linetype=2)+
  facet_grid(Dev~Class)+
  theme_bw()+#ylim(0,0.75)+
  scale_color_hue(l=40)+
  theme(legend.position=c(0.15,0.93), legend.key=element_blank(),
        legend.background=element_rect(fill='transparent'))+
  labs(x='Year',y='Probability of developing ESRD',color='')

pdf('graphsForPaper2/Figure4.pdf',width=10, height=6)
print(pltpanel)
dev.off()


# Supplemental figure 3 ---------------------------------------------------
## See indivEsts.R

# Supplemental figure 4 ---------------------------------------------------

pltResults2 <- function(bl){
  require(ggplot2)
  levels(bl$Dev) <- paste(levels(bl$Dev),'countries')
  levels(bl$Year) <- gsub('s','',levels(bl$Year))
  p=ggplot(bl,aes(x=yr,y=1-Med, group=Dev,color=Dev))+geom_line(size=1)+
    geom_line(aes(x=yr, y=1-LB),linetype=2)+geom_line(aes(x=yr, y=1-UB),linetype=2)+
    facet_wrap(~Year, ncol=1)+ylim(0,1)+scale_color_hue(l=40)+
    labs(x='Year',y='Probability of developing ESRD',color='')+
    theme_bw()+theme(legend.key=element_blank(), legend.position=c(0.8,0.95),
                     legend.background=element_rect(fill="transparent"))+xlim(1970,2015)
  return(p)
}

library(grid)
pdf('graphsForPaper2/SupplFigure4.pdf',width=6, height=9)
print(pltResults2(bl.full)+ylim(0,0.5)+theme(panel.margin=unit(2,'lines')))
dev.off()

# Supplemental Figure 5 ---------------------------------------------------

blah <- list('Observational'=blobs, 'Trial'=bltrial)
blah  <- ldply(blah, .id='Design')
levels(blah$Dev) <- paste(levels(blah$Dev),'countries')
levels(blah$Year) <- gsub('s','',levels(blah$Year))

pltpanel2 <- ggplot(blah, aes(x=yr, y=1-Med, group=Year,color=Year))+geom_line(size=1)+
  geom_line(aes(x=yr,y=1-LB), linetype=2)+geom_line(aes(x=yr, y=1-UB), linetype=2)+
  facet_grid(Dev~Design)+ scale_color_hue(l=40)+
  labs(x='Year',y='Probabilty of developing ESRD', color='')+
  theme_bw()+
  theme(legend.key=element_blank(), legend.background=element_rect(fill='transparent'))+
  xlim(1970,2015)

pdf('graphsForPaper2/SupplFigure5.pdf',width=7, height=7)
print(pltpanel2+theme(legend.position=c(0.15,0.9)))
dev.off()

# Supplemental Figure 6 ---------------------------------------------------

blhq <- subset(blhq, Dev=='Developed')
levels(blhq$Year) <- gsub('s','',levels(blhq$Year))
plthq <- ggplot(blhq, aes(x=yr, y=1-Med, group=Year, color=Year))+geom_line(size=1)+
  geom_line(aes(x=yr, y=1-LB),linetype=2)+geom_line(aes(x=yr, y=1-UB), linetype=2)+
  scale_color_hue(l=40)+
  labs(x='Year',y='Probability of developing ESRD', color='')+
  theme_bw()+
  theme(legend.key=element_blank(), legend.background=element_rect(fill='transparent'),
        legend.position=c(0.8,0.8))+
  xlim(1970,2015)

pdf('graphsForPaper2/SupplFigure6.pdf',width=7, height=7)
print(plthq+ylim(0,0.5))
dev.off()


# Supplemental Figure 8 ---------------------------------------------------

load('data/rda/bookkeeping.rda')
load('data/rda/WardTableFinal.rda')

lags.by.window <- lapply(members,get.lag, basedata )
nrisk.by.window <- lapply(members, get.n.risk, basedata, arm='FullStudy', developed=T)
sCr.by.window <- lapply(members, get.sCr, basedata, arm='FullStudy')

tmp <- mapply(cbind, nrisk.by.window, lags.by.window, SIMPLIFY=FALSE)
by.window <- mapply(cbind,tmp, sCr.by.window, SIMPLIFY=FALSE)
names(by.window) <- as.character(Windows[,3])

require(plyr)
out <- ldply(by.window, ddply, ~Developed, 
             function(x) sapply(x[,c('sCr','lags')],weighted.mean, w=x$number, na.rm=T)
            )
names(out)[1] <- 'yr'
out$yr <- as.numeric((out$yr))

## SI units
out <- mutate(out, sCrSI = sCr * 88.4)


library(ggplot2)
library(reshape2)
p1 <- ggplot(out, aes(x=yr, y=sCr, group=Developed, color=Developed))+
  geom_line(size=1)+
  ylim(c(0,2))+
  labs(x='Year',y='Serum Creatinine (mg/dl)', color='')+
  scale_color_hue(l=40)+
  theme_bw()+
  theme(legend.background=element_rect(fill='transparent'), legend.position=c(0.2,0.9),
        legend.key=element_blank())
p1a <- ggplot(out, aes(x=yr, y=sCrSI, group=Developed, color=Developed))+
  geom_line(size=1)+
  labs(x='Year',y='Serum Creatinine (micromoles/L)',color='')+
  scale_color_hue(l=40)+
  theme_bw()+
  theme(legend.background=element_rect(fill='transparent'), legend.position=c(0.2,0.9),
        legend.key=element_blank())


p2 <- ggplot(out, aes(x=yr, y=lags, group=Developed, color=Developed))+
  geom_line(size=1)+
  scale_y_continuous(breaks=0:10,
                     limits=c(0,2))+
  scale_color_hue(l=40)+
  labs(x='Year', y='Years since onset',color='')+
  theme_bw()+
  theme(legend.background=element_rect(fill='transparent'),
        legend.key=element_blank(), legend.position=c(0.2,0.9))

pdf('graphsForPaper2/SupplFigure8a.pdf', width=7, height=7)
print(p1+ylim(0.8,2))
dev.off()
pdf('graphsForPaper2/SupplFigure8aSI.pdf', width=7, height=7)
print(p1a+ylim(75,200))
dev.off()
pdf('graphsForPaper2/SupplFigure8b.pdf',width=7, height=7)
print(p2+ylim(0,1.7))
dev.off()


# Package in a single file ------------------------------------------------



# Confirmation of results numbers -----------------------------------------

## Countries & Continents
# continents <- list(
#   "Europe" = c('Belgium','Bosnia and Herzegovina','Czech Republic','Denmark','France','Germany','Greece','Hungary','Italy',
#                'Netherlands','Norway','Poland','Portugal','Serbia','Slovenia','Spain','Sweden','Turkey','UK','United Kingdom'),
#   "Asia" = c('China','India','Japan','Malaysia','Pakistan','Singapore','South Korea','Taiwan','Thailand'),
#   "Africa" = c("Morocco",'Saudi Arabia','Senegal','South Africa','Tunisia'),
#   "Latin America" = c('Brazil','Chile','Curacao','Latin America','Martinique','Mexico'))
# 
# ## Results from figures
# blah <- subset(bl, Dev=='Developing' & Year=='15 years')
# blah[,3:5] <- 1-blah[,3:5]
# summary(blah$Med[blah$yr < 1980])
# summary(blah$Med[blah$yr %in% 1993:1997])
# blah <- subset(bl, Dev=='Developed' & Year=='15 years')
# blah[,3:5] <- 1-blah[,3:5]
# summary(blah$Med[blah$yr %in% 1993:1997])
# 
# 
# Extract credible regions ----------------------------------------------------
out <- singleCR(1995, project='FullMixed', level=0.95)

out <- pooledCR(2000,2010, project='FullMixed', level=0.95)


###########################################################################
# Paper revision code -----------------------------------------------------
###########################################################################

# Biopsied fraction
load('data/rda/WardTableFinal.rda')
basedata %>%
  dplyr::filter(Arm=='FullStudy' & Group_class=='Mixed') %>% #subset
  select(number, bx., Developed) %>% # keep pertinent variables
  group_by(Developed) %>% # Group by
  summarize(NumberOfStudies=length(bx.), missing=sum(is.na(bx.)), 
            avg=weighted.mean(bx., number, na.rm=T)) # Summarize

# Ethnic composition over time
load('data/rda/bookkeeping.rda')
bl <- basedata %>%
  dplyr::filter(Arm=='FullStudy' & Group_class=='Mixed') %>%
  select(study, number, white, black, asian, hisp)

window_eth <- lapply(by.window, function(x) left_join(x, bl))
for(i in 1:length(window_eth)) window_eth[[i]]$window <- i
names(window_eth) <- Windows[,3]

bl2 <- llply(window_eth, function(x) x %>% group_by(Developed) %>% # by developed status
        summarise(window=unique(window),
                  white=weighted.mean(white,number, na.rm=T),
                  black=weighted.mean(black, number, na.rm=T),
                  asian=weighted.mean(asian, number, na.rm=T),
                  hisp = weighted.mean(hisp, number, na.rm=T))) %>%
  ldply(., .id='yr')  %>%  # Transform into data.frame, add names as a variable
  mutate(total=white+black+asian+hisp, white=white/total*100, black=black/total*100, 
              asian=asian/total*100, hisp=hisp/total*100) %>% # Normalize to 100
  select(-total) # Remove total

bl3 <- melt(bl2, id.vars=c('Developed','window','yr'))
levels(bl3$variable) <- c('White','Black','Asian','Hispanic')
require(ggplot2)
plt <- ggplot(bl3, aes(x=as.numeric(as.character(yr)), y=value, group=variable, color=variable,fill=variable))+
  geom_line()+
  xlab('Year')+ylab('Percentage')+ylim(0,100)+
  facet_wrap(~Developed, ncol=1)+labs(color='Ethnicity')
pdf('graphsForPaper2/SupplFigure1.pdf')
print(plt)
dev.off()


# Sensitivity analysis ----------------------------------------------------

## Extract MCMC results (see MovingAverage.R for run code)
basedir = file.path('data','mcmc')

bl.orig <- collapseResults(mcmcout('FullMixed',basedir))
pltResults(bl.orig)+ylim(0,0.6)
ggsave('graphsForPaper2/Fig2.pdf')

bl.sens1 <- collapseResults(mcmcout('FullMixedSensitivity', basedir)) # Start date at 3/4
bl.sens2 <- collapseResults(mcmcout('FullMixedSensitivity2',basedir)) # 10 year credit
pltResults(bl.sens2)+ylim(0,0.6)
ggsave('graphsForPaper2/SupplFig6.pdf')

bl.class3.sens <- collapseResults(mcmcout('class3Sensitivity',basedir))
bl.class4.sens <- collapseResults(mcmcout('class4Sensitivity',basedir))
bl.class5.sens <- collapseResults(mcmcout('class5Sensitivity', basedir))

bl.class3 <- collapseResults(mcmcout('class3',basedir))
bl.class4 <- collapseResults(mcmcout('class4',basedir))
bl.class5 <- collapseResults(mcmcout('class5',basedir))

pdf('ClassSpecificSensitivity.pdf')
pltResults(bl.class3)+ggtitle('Class 3 original')+ylim(0,0.8)
pltResults(bl.class3.sens)+ggtitle('Class 3 sensitivity')+ylim(0,0.8)
pltResults(bl.class4)+ggtitle('Class 4 original')+ylim(0,0.8)
pltResults(bl.class4.sens)+ggtitle('Class 4 sensitivity')+ylim(0,0.8)
pltResults(bl.class5)+ggtitle('Class 5 original')+ylim(0,0.8)
pltResults(bl.class5.sens)+ggtitle('Class 5 sensitivity')+ylim(0,0.8)
dev.off()

pltResults(bl.sens2)

# Supplemental figure 2 &  9 (Staircase plots) ---------------------------------


load('data/rda/WardTableFinal.rda')
load('data/rda/bookkeeping.rda')
dat <- basedata %>% 
  mutate(starts=beginYear(), ends=endYear(), yr_of_study=startYear(), 
         yr_of_study_end=endYear())
stairdat <- stairdata(dat, arm='FullStudy', group='Mixed')
pdf('graphsForPaper2/SupplFigure2a.pdf', width=7, height=7)
stairplot(dplyr::filter(stairdat, Developed=='Developed'))
dev.off()
pdf('graphsForPaper2/SupplFigure2b.pdf', width=7, height=7)
stairplot(dplyr::filter(stairdat, Developed=='Developing'))
dev.off()

dat <- basedata %>%
  mutate(starts=beginYear(), ends=endYear(), yr_of_study=startYear(), 
         yr_of_study_end=endYear(maxduration=10))
stairdat <- stairdata(dat, arm='FullStudy',group='Mixed')
stairplot(dplyr::filter(stairdat, Developed=='Developed'))
ggsave('graphsForPaper2/SupplFigure9a.pdf')
stairplot(dplyr::filter(stairdat, Developed=='Developing'))
ggsave('graphsForPaper2/SupplFigure9b.pdf')

## Class-specific staircase plots
dat <- basedata %>%
  mutate(starts=beginYear(), ends=endYear(), yr_of_study=startYear(), 
         yr_of_study_end=endYear(maxduration=10))
stdat <- stairdata(dat, arm='', group=c('III','IV','V'))
stdat$labels <- paste0(stdat$labels, ' (',stdat$Group_class,')')
stdat <- ddply(stdat, ~labels, function(x){
  if(nrow(x)>1){
    y = x[1,]
    y$number <- sum(x$number)
    return(y)
  } else {
    return(x)
  }
}) %>% arrange(starts)
stairplot(dplyr::filter(stdat, Developed=='Developed'), lims = c(1955, 2013))
ggsave('graphsForPaper2/SupplFig10a.pdf')
stairplot(dplyr::filter(stdat, Developed=='Developing'), lims=c(1965,2015))
ggsave('graphsForPaper2/SupplFig10b.pdf')


# Number of studies. ------------------------------------------------------

## Get study id's for different analyses from MovingAverage.R

# load('data/rda/WardTableFinal.rda')
# load('data/rda/class3.rda')
# load('data/rda/class4.rda')
# load('data/rda/class5.rda')
# library(dplyr)
# 
# FullMixed <- basedata %>% 
#   dplyr::filter(Arm=='FullStudy',Group_class=='Mixed') %>% 
#   select(study, Developed) %>% 
#   count(Developed)
# 
# classcount <- dplyr::left_join(class3data %>% count(Developed), class4data %>% count(Developed),by='Developed') %>% dplyr::left_join(class5data %>% count(Developed), by='Developed')
# dplyr::left_join(FullMixed, classcount, by='Developed')
# numbers <- dplyr::left_join(FullMixed, classcount, by='Developed')
# names(numbers)[-1] <- c('Full','Class 3','Class 4','Class 5')


# Class-specific sensitivity ----------------------------------------------

# bl3_1 <- collapseResults(mcmcout('class3', basedir))
# bl4_1 <- collapseResults(mcmcout('class4', basedir))
# bl5_1 <- collapseResults(mcmcout('class5', basedir))
# 
# blah <- list('Class III'=bl3_1, "Class IV" = bl4_1, "Class V" = bl5_1)
# bl.all <- ldply(blah, .id='Class')
# levels(bl.all$Dev) <- paste(levels(bl.all$Dev),'countries')
# levels(bl.all$Year) <- gsub('s','',levels(bl.all$Year))
# 
# pltpanel <- ggplot(bl.all, aes(x=yr, y=1-Med, group=Year, color=Year))+geom_line(size=1)+
#   geom_line(aes(x=yr, y=1-LB),linetype=2)+
#   geom_line(aes(x=yr, y=1-UB), linetype=2)+
#   facet_grid(Dev~Class)+
#   theme_bw()+#ylim(0,0.75)+
#   scale_color_hue(l=40)+
#   theme(legend.position=c(0.15,0.93), legend.key=element_blank(),
#         legend.background=element_rect(fill='transparent'))+
#   labs(x='Year',y='Probability of developing ESRD',color='')
# 
# pdf('graphsForPaper2/Figure4.pdf',width=10, height=6)
# print(pltpanel)
# dev.off()
# 
