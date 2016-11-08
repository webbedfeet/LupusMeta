setwd("E:/Work/Ward/Studies/LupusMeta")

# Create a sandbox for file manipulation ----------------------------------

origdir <- 'data/fromPapers'
datadir <- 'data/sandbox'
dir.create(datadir)
csvfiles = dir(origdir, pattern='csv')
for(f in csvfiles){
  file.copy(file.path(origdir,f),file.path(datadir,f), overwrite=T)
}

# Normalize KM data -------------------------------------------------------

for(i in 1:length(csvfiles)){
  bl <- read.csv(file.path(datadir,csvfiles[i]), header=F, stringsAsFactors=F)
  if(ncol(bl)>3) stop(paste("Too many columns in", csvfiles[i]))
  if(!is.character(bl[1,2])){
    bl <- read.csv(file.path(datadir, csvfiles[i]), header=T, stringsAsFactors=F)
    if(ncol(bl)==2) names(bl) <- c('Year','Prob')
    if(ncol(bl)==3) names(bl) <- c('Group','Year','Prob')
  } else {
    bl <- bl[-1,]
    if(ncol(bl)==2) names(bl) <- c('Year','Prob')
    if(ncol(bl)==3) names(bl) <- c('Group','Year','Prob')
  }
  write.csv(bl, file=file.path(datadir,csvfiles[i]), row.names=F)
}

# conversion to numeric ---------------------------------------------------

for(i in 1:length(csvfiles)){
  bl <- read.csv(file.path(datadir, csvfiles[i]), header=T)
  bl$Year <- as.numeric(bl$Year)
  bl$Prob <- as.numeric(bl$Prob)
  write.table(bl, file=file.path(datadir, csvfiles[i]), row.names=F, sep=',')
}

# Manual validation done 5/23/2013
# Archive original data before copying
system("zip -j data/OriginalKMextract.zip data/fromPapers/*") # -j strips the path name

# Copy back ---------------------------------------------------------------

for(f in csvfiles){
  file.copy(file.path(datadir, f), file.path(origdir,f), overwrite=T)
}

# Remove sandbox ----------------------------------------------------------

for(f in dir(datadir)){
  file.remove(file.path(datadir,f))
}
system(paste('rmdir',datadir))
