
# Preamble ----------------------------------------------------------------
options(stringsAsFactors=F)
if('fn' %in% search()) detach('fn')
fn <- new.env()
source('functions.R',local=fn)
attach(fn)
library(dplyr)
library(readxl)


# Read original data ------------------------------------------------------


# basedata <- read.xls('data/Lupus_ESRD_ward28_max.xls', stringsAsFactors=F)
# basedata <- basedata[,-grep('^X',names(basedata))] 
# 
# save(basedata, file='data/WardTable.rda',compress=T)



# Loading raw data --------------------------------------------------------

load('data/rda/WardTable.rda') # provides basedata
# load('data/rda/kmdata.rda') # provides kmdata2
## Adding updated studies from 2013. 
untar('data/xls.tar.gz',exdir='data')
newdat <- read.xls('data/xls/Data abstraction 2013 articles.xlsx')
names(newdat) <- names(basedata)
basedata <- rbind(basedata, newdat)
newdat <- read.xls('data/xls/pons-estel.xlsx')
names(newdat)  <- names(basedata)
basedata <- rbind(basedata,newdat)
## Update April 2015
newdat <- read_excel('data/ESRD_2015update.xlsx')
newdat <- newdat[,which(!is.na(names(newdat)))]
names(newdat) <- make.names(names(newdat))
basedata <- rbind(basedata, newdat)
basedata <- basedata %>% dplyr::filter(!(study %in% c(69, 140))) # Replaced studies.
basedata <- basedata %>% dplyr::filter(study != 196) # Remove Alaiya since total events not available.
basedata <- basedata %>% dplyr::filter(study != 187) # Arends has a problem in how I'm creating data

# Updated data ------------------------------------------------------------

basedata[basedata$Author=='Bardana','maxfollowup'] <- 276
basedata[basedata$Author=='Niang','maxfollowup'] <- 24
basedata[basedata$Author=='Zavada','maxfollowup'] <- 76
basedata[basedata$Author=='Moroni' & basedata$pubdate=='2012','maxfollowup'] <- 229
basedata[basedata$study==168,'maxfollowup'] <- 144
basedata[basedata$study==171,'maxfollowup'] <- 180
basedata[basedata$study==179, 'maxfollowup'] <- 420
basedata[basedata$study==189, 'maxfollowup'] <- 'sd 14.1'
basedata[basedata$study==197, 'maxfollowup'] <- 'sd 30'

# Normalize Author --------------------------------------------------------
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

basedata$Author <- capwords(as.character(basedata$Author), strict=T)
names(basedata)[grep('other$',names(basedata))] <- 'otherrace'

# Identify arms representing the full study -------------------------------

basedata$Arm[basedata$Arm==''] <- 'FullStudy'
basedata$Arm[is.na(basedata$Arm)] <- 'FullStudy'
basedata$Arm[str_detect(basedata$Arm, '^[0-9]+$')] <- 'FullStudy' # Only number, no arm

# ID single class vs overall arms -----------------------------------------

classdata <- basedata[,grep('class[1-6]$',names(basedata))]
singleclass  <- apply(classdata,1, function(x) any(x==100))

# ID single drug combination vs mixed drug regimens -----------------------

drugdata <- basedata[,c('pred','gconly','ctxnih','ctxeuro','ctxpo','aza','mmf','csa','rtx','othermed')]
drugmiss <- apply(drugdata,1,function(x) all(is.na(x)))
singledrug <- apply(drugdata,1,function(x) all(x==100|x==0, na.rm=T))
singledrug[which(drugmiss)] <- NA

# Single gender studies

singlegender <- basedata$female %in% c(0,100)
singlegender[is.na(basedata$female)] <- NA

groups <- plyr::count(data.frame(singleclass,singledrug,singlegender))

# Single race studies

racedata <- basedata[,c('white','black','asian','hisp','otherrace')]
racemiss <- apply(racedata,1,function(x) all(is.na(x)))
singlerace <- apply(racedata,1,function(x) all(x==100|x==0,na.rm=T))
singlerace[which(racemiss)] <- NA



# Update maxfollowup ------------------------------------------------------

follow <- as.character(basedata$maxfollowup)
ind.sd <- grep('sd',follow)
sds <- gsub('sd ','',
            sapply(
              strsplit(follow[ind.sd],';'), function(x) x[grep('sd',x)])
)
follow[ind.sd] <- as.character(basedata$esrdfu..m.[ind.sd]+3*as.numeric(sds)) # Estimate followup by mean + 3SD
follow <- as.numeric(follow)
basedata$maxfollowup  <- follow

### 152/161 or 94.4% of the studies have either max followup or survival risk data
### available. 

# Compute study year as middle of enrollment ------------------------------

timedat <- basedata[,c('start','endfu','endenroll')]
yr_of_study <- round(with(timedat, (start + pmin(endfu, endenroll, na.rm=T))/2))
ind <- which(is.na(yr_of_study))
yr_of_study[ind] <- ifelse(is.na(timedat$start[ind]),NA, round((basedata$pubdate[ind]-1+timedat$start[ind])/2))
ind <- which(is.na(yr_of_study))
yr_of_study[ind] <- round(pmin(basedata$endfu[ind],basedata$pubdate[ind]-1,na.rm=T) - pmax(basedata$esrdfu..m.[ind],basedata$maxfollowup[ind],na.rm=T)/12/2)
ind <- is.na(yr_of_study) # All left is study 118
yr_of_study[ind] <- rep(yr_of_study[basedata$study==118 & !ind],sum(ind))


# Determine income status of country --------------------------------------
library(stringr)
options(stringsAsFactors=F)
thresh <- read.xls('data/xls/OGHIST.xls',sheet='Thresholds',skip=6, stringsAsFactors=F)
names(thresh) <- gsub('X','',names(thresh))
thresholds <- data.frame(cbind(names(thresh)[-1], t(thresh[14,-1])))
for(i in 1:2) thresholds[,i] <- as.character(thresholds[,i])
thresholds[,2] <- str_trim(str_replace(thresholds[,2],'>',''))
thresholds[,2] <- str_replace(thresholds[,2],',','')
thresholds[,2] <- as.numeric(thresholds[,2])
thresholds <- subset(thresholds, V1 %in% as.character(1987:2011))
bl <- thresholds[,2]; names(bl) <- thresholds[,1]

incomes <- read.xls('data/xls/NY.GNP.PCAP.CD_Indicator_MetaData_en_EXCEL.xls', stringsAsFactors=F)
incomes <- incomes[-(1:32),] # Remove continents
names(incomes) <- str_replace(names(incomes),'X','')
incomes$Country.Name[incomes$Country.Name=='United States'] <- "US"
incomes$Country.Name[incomes$Country.Name=='United Kingdom'] <- "UK"
incomes$Country.Name[incomes$Country.Name=='Korea, Rep.'] <- "South Korea"
incomes$Country.Name[incomes$Country.Name=='Iran, Islamic Rep.'] <- "Iran"
incomes$Country.Name[incomes$Country.Name=='Egypt, Arab Rep.'] <- "Egypt"

incomes <- subset(incomes, Country.Name %in% basedata$Country)
incomes <- cbind(Country.Name=incomes$Country.Name, incomes[,as.character(1987:2011)])

develop.type <- mapply(function(x,y) ifelse(x>y, 'Developed','Developing'),incomes[,-1], bl )
develop.type <- data.frame(Country.Name=incomes$Country.Name, develop.type, stringsAsFactors=F)
names(develop.type) <- str_replace(names(develop.type), 'X','')
row.names(develop.type) <- develop.type$Country.Name
develop.type <- develop.type[,-1]
develop.type["Bosnia and Herzegovina",] <- rep('Developed',ncol(develop.type))

Developed <- rep('',nrow(basedata))
for(i in 1:nrow(basedata)){
  Developed[i] <- develop.type[basedata$Country[i], as.character(max(1987,yr_of_study[i]))]
}
Developed[basedata$Country=='Europe/North America'] <- 'Developed'
Developed[basedata$Country=='Slovenia' & yr_of_study < 1992] <- 'Developing'
Developed[basedata$Country=='Serbia' & yr_of_study < 1999] <- 'Developing'
Developed[basedata$Country=='Taiwan'] <- 'Developed'
Developed[basedata$Country=='Canada and Jamaica'] <- 'Developed'
Developed[basedata$Country=='Martinique'] <- 'Developed'
Developed[basedata$Country=='Poland' & yr_of_study < 1992] <- 'Developing'
Developed[basedata$Country=='Czech Republic' & yr_of_study < 2002] <- 'Developing'
Developed[basedata$Country=='Curacao'] <- 'Developed'
Developed[basedata$Country=='United Kingdom'] <- 'Developed'
Developed[basedata$Country=='Latin America'] <- 'Developing'


# Decades -----------------------------------------------------------------

Decades <- cut(yr_of_study, c(1950,seq(1980,2010, by=10)), include.lowest=T, right=F)
levels(Decades) <- c('< 1979',paste(seq(1980,2000,by=10),seq(1989,2009,by=10), sep='-'))

YrGrp <- cut(yr_of_study, seq(1960,2010,by=5), include.lowest=TRUE, right=FALSE)
levels(YrGrp) <- paste(seq(1960,2005, by=5), seq(1964, 2009, by=5), sep='-')

# Availability of survival and at risk data -------------------------------

yearvars <- grep('esrdy[0-9]+$',names(basedata),value=T)
atriskvars <- grep('esrdy[0-9]+number',names(basedata),value=T)

yr_avail <- apply(basedata[,yearvars],1,function(x)any(!is.na(x)))
at_risk_avail <- apply(basedata[,atriskvars],1,function(x) any(!is.na(x)))


# Determining left truncation ---------------------------------------------

basedata$lags <- with(basedata, pmin(sledur..yr.,LNdur, na.rm=T))
basedata$lags[basedata$Time0 %in% c('Biopsy','Diagnosis')] <- 0
basedata$lags <- ifelse(is.na(basedata$lags),0,basedata$lags)

# Putting study data all together -----------------------------------------

basedata <- cbind(basedata, singleclass,singledrug,singlegender, singlerace, yr_avail, at_risk_avail,
                  yr_of_study, Developed, Decades)
# Remove Ayodele 2010 and Xu, 2011 and Melender
basedata <- dplyr::filter(basedata, study !=7,study!=135, study!=150 )

basedata[basedata$study==181, 'Developed'] <- 'Developing'

#save(basedata, file='data/rda/WardTableFinal.rda',compress=T)


# ID groups for each arm --------------------------------------------------

Group <- rep("",nrow(basedata))
classvars <- paste('class',2:6,sep="")
drugvars  <- names(basedata)[38:47]
gender <- 'female'
racevars <- c('white','black','asian','hisp','otherrace')

dat <- basedata[,c('study','Arm',classvars,drugvars,racevars)]
require(reshape2)
require(plyr)

dat2 <- melt(dat, id=1:2)
dat2$variable <- as.character(dat2$variable)
blah  <- ddply(dat2, ~study+Arm, summarise, Group=ifelse(any(value==100 & !is.na(value)),paste(variable[value==100 & !is.na(value)],collapse=','),''))
#blah$Group[is.na(blah$Group)] <- 'Overall'

whichmember <- function(x, set){
  # is any member of the set in any element of x
  d1 <- sapply(set, function(y) str_extract(x,y))
  out <- apply(d1, 1, function(y) ifelse(all(is.na(y)),NA, paste(y[!is.na(y)], collapse=',')))
  return(out)
}

blah <- ddply(blah, ~study, transform, Group = paste(Group, Group[Arm=='FullStudy'],sep=','))
blah$Group <- unduplicate(blah$Group)

blah <- transform(blah, Group_class = whichmember(Group, classvars),
                  Group_drug = whichmember(Group, drugvars),
                  Group_race = whichmember(Group, racevars))
blah[is.na(blah)] <- 'Mixed'

### For Pasqualli (Study 113), making the mixed class4/class5 as a class4, pending further confirmation
### It is confirmed that class4/class5 should be class5
blah$Group_class[blah$Group_class=='class4,class5'] <- 'class5'
g <- as.factor(blah$Group_class)
levels(g) <- c('II','III','IV','V','VI','Mixed')
blah$Group_class <- as.character(g)


basedata <- join(basedata, blah, by=c('study','Arm'))
basedata$Decades <- as.character(basedata$Decades)

save(basedata, file='data/rda/WardTableFinal.rda',compress=T)

# Generate survival year data ---------------------------------------------
library(doBy)
authors <- with(basedata, sapply(split(paste(Author,pubdate),study),unique))

dat <- basedata[,c('study','Arm','Group_class','Group_drug','Group_race',names(basedata)[str_detect(names(basedata),'esrdy[0-9]+$')])]
dat2 <- melt(dat,id=c(3,4,5,1,2))
bl <- orderBy( ~study+Arm+Group_class+Group_drug+Group_race+variable,data=dat2)
bl$value <- 1-as.numeric(bl$value)/100
bl <- subset(bl, !is.na(value))
bl <- transform(bl, Year= as.numeric(str_extract(variable,'[0-9]+')))
names(bl) <- c('Group_class','Group_drug','Group_race','study','Arm','variable','Prob','Year')
survival_data <- dlply(bl[,c('study','Arm','Group_class','Group_drug','Group_race','Year','Prob')],~study,strip_splits)
# names(survival_data) <- authors[names(survival_data)]

# Generate at risk data ---------------------------------------------------

dat <- basedata[,c('study','Arm','Group_class','Group_drug','Group_race',names(basedata)[str_detect(names(basedata),'esrdy[0-9]+number')])]
dat2 <- melt(dat,id=c(3,4,5,1,2))
bl <- orderBy(~study+Arm+Group_class+Group_drug+Group_race+variable, data=dat2)
bl <- subset(bl, !is.na(value))
bl <- transform(bl, Year = as.numeric(str_extract(variable, '[0-9]+')))
names(bl) <- c('Group_class','Group_drug','Group_race','study','Arm','variable','Number.at.risk','Year')
atrisk_data <- dlply(bl[,c('study','Arm','Group_class','Group_drug','Group_race','Year','Number.at.risk')],~study, strip_splits)
#atrisk_data <- lapply(atrisk_data, function(m) if(length(unique(m$Group))==1){return(m[,-1])} else {return(m)})
# names(atrisk_data) <- authors[names(atrisk_data)]

save(survival_data, atrisk_data, file='data/rda/survival1.rda')


# Create KM data object indexed by study number ---------------------------
options(stringsAsFactors=F)
KMinfo <- read.xls('data/Summary2.xlsx',sheet='Summary')
untar('data/fromPapers.tar.gz')
n <- nrow(KMinfo)
kmdata <- list()
for(i in 1:n){
  kmdata[[as.character(KMinfo$studyno[i])]] <- read.csv(file.path('data','fromPapers',KMinfo$File[i]))
}
for(i in 1:n){
  if(ncol(kmdata[[i]])==2){
    kmdata[[i]] <- data.frame(Group=rep('FullStudy',nrow(kmdata[[i]])), kmdata[[i]])
  }
}

# at_risk data
paper_at_risk <- list()
for(i in 1:nrow(KMinfo)){
  if(KMinfo$Details[i]=='Y'){
    paper_at_risk[[as.character(KMinfo$studyno[i])]] <- read.xls('data/Summary2.xlsx',sheet=KMinfo$Sheet[i])
  }
}
### Valeri is weird right now, not including (7/7/13)


kmdata_atrisk <- paper_at_risk

kmdata <-kmdata[-which(names(kmdata) %in% c('7','38','53'))] # remove Ayodele 2010 and Daniel and Faurschou from KM data
kmdata[['138']] <- kmdata[['138']][-239,]# Bad data record
save(kmdata, kmdata_atrisk, file='data/rda/kmdata2.rda',compress=T)


# Create data object for only summary survival data -----------------------

onlySummary <- setdiff(names(survival_data),names(kmdata)) 
summaryData <- survival_data[onlySummary]
summaryData_atrisk <- atrisk_data[setdiff(names(atrisk_data),names(kmdata))]
save(summaryData, summaryData_atrisk, file='data/rda/summarySurv.rda',compress=T)

# Create data objects for studies with only followup data -----------------
basedata <- transform(basedata, Events = round(esrd*number/100))
save(basedata, file='data/rda/WardTableFinal.rda',compress=T)

tmp <- basedata[,c('study','Arm', 'Author','Group_class','Group_drug','Group_race','maxfollowup','Events')]
tmp <- subset(tmp, !(study %in% union(names(kmdata),names(summaryData))))
tmp <- tmp[!is.na(tmp$maxfollowup),]
followup_data <- dlply(tmp, ~study, strip_splits)
### No at risk data here

save(followup_data, file='data/rda/followup.rda',compress=T)

## There are 3 studies with no KM, summary or max followup. 
## They are:
# study       Author                              Citation
# 52     18       Brugos        Scand J Immunol 2006; 64:433-7
# 115    50        Erdem                  Nephron 1996; 72:332
# 227   117      Rabbani                  JPMA 2005; 55:328-32

unlink('data/xls',recursive=TRUE)
unlink('data/fromPapers',recursive=TRUE)


# Identify high-quality studies -------------------------------------------

## High quality studies are those with inception='inception' and bx < 100% and ExcludeCRF='no'
require(stringr)
basedata$inception <-  str_trim(tolower(basedata$inception))
save(basedata, file='data/rda/WardTableFinal.rda',compress=T)

basedata.hq <- dplyr::filter(basedata, inception=='inception',bx.<100, !is.na(bx.), ExcludeCRF=='no', Developed== 'Developed')
ind <- setdiff(basedata$study, basedata.hq$study)
basedata.lq <- dplyr::filter(basedata, study %in% ind)

# Fixing hyphenated author names -----------------------------------------------------

hyphenCap <- function(x){
  s <- strsplit(x, '-')[[1]]
  paste(toupper(substring(s,1,1)), substring(s,2), sep='', collapse='-')
}

basedata$Author <- sapply(basedata$Author, hyphenCap)

save(basedata, file='data/rda/WardTableFinal.rda', compress=T)
