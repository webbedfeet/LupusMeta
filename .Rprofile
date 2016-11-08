# Initialization file

.First <- function(){
  if(R.version$major=='3' & R.version$os=='mingw32') .libPaths('P:/R/win-library/3.0')
  reload()
#  print('Local Rprofile loaded')
}
reload <- function(){
  if('fn' %in% search()) detach('fn')
  fn <- new.env()
  source('lib/load_libraries.R')
  # source('lib/functions.R',local=fn)
  # source('lib/truncFunctions.R',local=fn)
  # source('lib/KMcleaning.R', local=fn)
  attach(fn)    
}
