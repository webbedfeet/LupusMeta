# Running the full analysis -----------------------------------------------

reload()

# Data preparation --------------------------------------------------------

source('DataManagement.R')
source('DataMunging.R')
source('SurvivalTimes.R')
source('SummarySurv2IPD.R')
# source('KMmodifications.R') # done on original data
# source('Reorgnize.R') # Cleaning data structure to have one KM per study

# Preparing the calendar windows for moving average -----------------------

source('baseline.R')

# Preparing IPD and followup data by class --------------------------------

source('BayesianMixed.R')
source('BayesianClass3.R')
source('BayesianClass4.R')
source('BayesianClass5.R')

# Creating JAGS-compatible data for moving average  -----------------------

source('MovingAverage.R')

# Producing summaries and plots from Bayesian analysis --------------------

source('OutputForPaper.R')
source('indivEsts.R')
