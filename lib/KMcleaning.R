#' Clean digitally captured Kaplan-Meier plots
#' 
#' \code{KMclean} takes data from a graph digitizer and corrects it by enforcing
#' basic properties of a Kaplan-Meier curve. These include the curve starting at
#' (0,1), having a minimum step size of 1/n where n is the number at risk at the
#' start of the study, and being left-continuous. Input data can be in 2 columns
#' (Time, survival probability) or 3 columns (Group, Time, survival 
#' probability). \code{KMclean} will clean the data to conform to the properties
#' of KM curves, splitting the data by groups if available.
#' 
#' @param dat A data.frame object containing the digitized plot. It can have 
#'   either 2 (Time, Prob) or 3 (Group, Time, Prob) columns. The specific
#'   ordering of the columns is assumed in the function.
#' @param n.risk Number at risk at start of study. In the case of grouped data, 
#'   it should be a vector with names corresponding to the group names (e.g.
#'   c("Grp1"=24, "Grp2"=129)) Otherwise, it is a scalar.
#' @param ... Additional inputs (See Details)
#' @return A data.frame (if dat is 2 columns) or list of data.frame objects (if 
#'   dat is 3 columns) with the cleaned KM curves
#' @export
#' 
KMclean <- function(dat, n.risk, start.time=0, start.p = 1, x.thresh=NULL){
  require(plyr, quietly=T)
  nc <- ncol(dat)
  dat <- dat[!is.na(dat[,nc]),]
  if(nc==3){
    names(dat) <- c('Group','Time','Prob')
    if(length(n.risk) != length(unique(dat[,1]))){
      ans <- readline('Should I repeat the number at risk? (Y/N): ')
      if(ans=='N'|ans=='n'){
        stop('Number at risk must be present for each group')
      } else {
        n.risk <- rep(n.risk, length(unique(dat[,1])))
        names(n.risk) <- unique(dat[,1])
      }
    }
    dat[,1] <- as.character(dat[,1])
    n.group <- length(unique(dat[,1]))
    if(n.group==1){
      out <- KMclean(dat[,-1], n.risk,start.time=start.time, start.p=start.p,
                     x.thresh=x.thresh)
    } else {
      lst <- dlply(dat,.(Group),strip_splits)
      out <- list()
      for(u in unique(dat$Group)){
        if(!(u %in% names(n.risk))) stop(paste("No number at risk available for",u))
        out[[u]] <- KMclean(lst[[u]],n.risk[u], start.time=start.time, 
                            start.p=start.p, x.thresh=x.thresh)
      }
    }
  }
  
  if(nc==2){ # Single arm
    names(dat) <- c('Time','Prob')
    if(is.null(x.thresh)){
      x.thresh <- diff(range(dat[,1], na.rm=T))/500 # ad-hoc change to 500
    }
    bl <- ceiling(log10(n.risk)) # Calibrates order of magnitude of n.risk
    minjump <- 1-floor((10^bl)*1/n.risk)/(10^bl) # reduces precision of threshold, given error in digitization
    
    n <- nrow(dat)
    o <- order(dat[,1])
    dat <- dat[o,] #Increasing order of time
    for(i in 1:(n-1)){
      if(abs(dat[i+1,1]-dat[i,1])<x.thresh & dat[i+1,2]>dat[i,2]){ # Fix idiosyncracies in digitization
        dat[i+1,1] <- dat[i,1]
        x <- dat[i+1,]
        dat[i+1,] <- dat[i,]
        dat[i,] <- x
      }
    }
    
    
    dat <- rbind(c(start.time,start.p), dat) # Start at (0,1)
    dat[,2] <- pmin(pmax(dat[,2],0),1) # ensures data is in [0,1]
    dat[,1] <- pmax(0,dat[,1]) # ensures time is non-negative
    dat[1,2] <- start.p # probabilities start at 1
    dat <- dat[!duplicated(dat),] # remove duplicate rows [do it again at end]
    
    for(i in 2:nrow(dat)){
      if(dat[i-1,2]-dat[i,2] <0){
        dat[i,2] <- dat[i-1,2] # non-increasing
      }
    }
    
    n <- nrow(dat)
    addon <- FALSE
    if((dat[n-1,2]>0 & dat[n,2]/dat[n-1,2]<minjump)){ # There exists a jump at the last step
      dat <- rbind(dat,c(dat[n,1]+x.thresh, dat[n,2]))
      addon <- TRUE
    }
    
    i <- 1
    while(i< n-1){
#       print(i)
      while(i < n-1 & (dat[i,2]>0 & dat[i+1,2]/dat[i,2] > minjump)){
        dat[i+1,2] <- dat[i,2] # If minjump not achieved, make a horizontal line
        i <- i+1
      }
      if(i>=n-1) break
      
      if(any(dat[1:n,1]>dat[i+1,1]+x.thresh)){
        i2 <- min(which(dat[1:n,1]>=dat[i+1,1]+x.thresh))
      } else {
        i2 <- min(which(dat[1:n,1]==max(dat[(i+2):n,1])))
      }
      if(i2 >= i+1){
        for(j in seq(i2,i+1,by=-1)){
          if(dat[j-1,2]>0 & dat[j,2]/dat[j-1,2]>minjump){
            dat[j-1,2] <- dat[j,2]
          }
        }
      }
      
      i3 <- min(which(dat[,2]==dat[i2,2]))
      if(i3>i+1 & dat[i+1,1] < dat[i,1]+x.thresh){
        dat[i+1,2]=dat[i3,2]
      }
      
      dat <- rbind(dat, c(dat[i+1,1],dat[i,2]))
      
      i <- i+1
    }
    
    o <- order(dat[,1],-dat[,2])
    dat <- dat[o,]
    
    if(addon) dat <- dat[-nrow(dat),]
    
    # take out multiple rows (more than 2) for the same time point. 
    dat <- ldply(split(dat, dat$Time), 
                 function(x){
                   if(nrow(x)>2){
                     return(data.frame(Time = rep(unique(x$Time),2),
                                       Prob = rev(range(x$Prob))))
                   } else {
                     return(x)
                   }
                 })[,-1] # Created additional col for split variable
    out <- dat[!duplicated(dat),]
    jumps <- abs(diff(out[,2]))
    jmp.times <- out[jumps>0,1]
    if(length(jmp.times)>0){
      if(max(jmp.times)==max(out[,1])){
        out <- rbind(out, c(out[nrow(out),1]+0.1,out[nrow(out),2]))
      }
    }
    # clean out unnecessary points
    ind <- which(duplicated(out[,1]))
    ind <- c(1, sort(c(ind-1,ind)), nrow(out))
    out <- out[ind,]
    out <- out[!duplicated(out),]
  } 
  return(out)
}

# Notes -------------------------------------------------------------------

# Issue remaining is that the minjump increases as n decreases, and we're still
# being forced to provide a death where the jump is insufficient for a death
#
# Do I account for it here or in the IPD generation?

