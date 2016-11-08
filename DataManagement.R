
# Data import -------------------------------------------------------------

library(gdata)
basedata <- read.xls('data/xls/Lupus_ESRD_ward28_max.xls')
basedata <- basedata[,-grep('^X',names(basedata))]

save(basedata, file='data/WardTable.rda',compress=T)

# Identify studies with KM and normalize PDF names ------------------------

bl <- basedata[,grep('^esrdy',names(basedata))]
bl <- !apply(bl,2,is.na)
ind <- which(apply(bl,1,any))
AuthWithKM <- sort(unique(basedata$Author[ind]))
pdffiles <- dir('CorePapers/',pattern='pdf')

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

for(i in 1:length(AuthWithKM)) AuthWithKM[i] <- capwords(AuthWithKM[i],strict=T)
pdffiles2 <- rep("",length(pdffiles))
for(i in 1:length(pdffiles)) pdffiles2[i] <- capwords(pdffiles[i], strict=T)